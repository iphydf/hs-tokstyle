load("@rules_cc//cc:action_names.bzl", "C_COMPILE_ACTION_NAME")
load("@rules_cc//cc:toolchain_utils.bzl", "find_cpp_toolchain")

def _tokstyle_c_test_impl(ctx):
    cc_toolchain = find_cpp_toolchain(ctx)
    feature_configuration = cc_common.configure_features(
        ctx = ctx,
        cc_toolchain = cc_toolchain,
        requested_features = ctx.features,
        unsupported_features = ctx.disabled_features,
    )
    c_compiler_path = cc_common.get_tool_for_action(
        feature_configuration = feature_configuration,
        action_name = C_COMPILE_ACTION_NAME,
    )

    exe = ctx.actions.declare_file(ctx.label.name)

    # Build the final command line
    test_args = []
    test_args.append("--cc=" + c_compiler_path)
    test_args.append("--include=_main/hs-tokstyle/include")

    for dep in ctx.attr.deps:
        if CcInfo in dep:
            comp_ctx = dep[CcInfo].compilation_context
            for inc in comp_ctx.includes.to_list():
                if inc.startswith("external/"):
                    test_args.append("-I" + inc[len("external/"):])
                else:
                    test_args.append("-I_main/" + inc)
            for inc in comp_ctx.system_includes.to_list():
                if inc.startswith("external/"):
                    test_args.append("-I" + inc[len("external/"):])
                else:
                    test_args.append("-I_main/" + inc)
            for inc in comp_ctx.quote_includes.to_list():
                if inc.startswith("external/"):
                    test_args.append("-I" + inc[len("external/"):])
                else:
                    test_args.append("-I_main/" + inc)

    test_args.extend(ctx.attr.copts)
    test_args.extend(["_main/" + f.short_path for f in ctx.files.srcs])

    # We use a shell script wrapper because it's a test
    ctx.actions.write(
        output = exe,
        content = """#!/bin/bash
cd ..
{tool} {args} "$@"
""".format(
            tool = "_main/" + ctx.executable._tool.short_path,
            args = " ".join(test_args),
        ),
        is_executable = True,
    )

    runfiles = ctx.runfiles(files = ctx.files.srcs + [ctx.executable._tool] + cc_toolchain.all_files.to_list())

    # Add dependencies runfiles (for headers)
    for dep in ctx.attr.deps:
        runfiles = runfiles.merge(dep[DefaultInfo].default_runfiles)
        if CcInfo in dep:
            # We also need the actual files for the preprocessor to find them
            runfiles = runfiles.merge(ctx.runfiles(files = dep[CcInfo].compilation_context.headers.to_list()))

    # Build the final command line
    test_args = []
    test_args.append("--cc=" + c_compiler_path)
    test_args.append("--include=hs-tokstyle/include")

    for dep in ctx.attr.deps:
        if CcInfo in dep:
            comp_ctx = dep[CcInfo].compilation_context
            for inc in comp_ctx.includes.to_list():
                test_args.append("-I" + inc)
            for inc in comp_ctx.system_includes.to_list():
                test_args.append("-I" + inc)
            for inc in comp_ctx.quote_includes.to_list():
                test_args.append("-I" + inc)

    test_args.extend(ctx.attr.copts)
    test_args.extend([f.short_path for f in ctx.files.srcs])

    return [DefaultInfo(
        executable = exe,
        runfiles = runfiles,
    )]

tokstyle_c_test = rule(
    implementation = _tokstyle_c_test_impl,
    test = True,
    attrs = {
        "srcs": attr.label_list(allow_files = True),
        "deps": attr.label_list(providers = [CcInfo]),
        "copts": attr.string_list(),
        "_tool": attr.label(
            default = Label("//hs-tokstyle/tools:check-c"),
            executable = True,
            cfg = "exec",
        ),
        "_cc_toolchain": attr.label(default = Label("@rules_cc//cc:current_cc_toolchain")),
    },
    fragments = ["cpp"],
    toolchains = ["@rules_cc//cc:toolchain_type"],
)
