#pragma once

enum {
  F_SETFD = 2, /* set/clear close_on_exec */
  F_GETFL = 3, /* get file->f_flags */
  F_SETFL = 4, /* set file->f_flags */
};

int fcntl(int fd, int cmd, ... /* arg */);
