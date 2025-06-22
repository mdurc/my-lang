#ifndef UTIL_H
#define UTIL_H

#include "logging/diagnostic.h"

#define _assert(cond, msg)                               \
  do {                                                   \
    if (!(cond)) {                                       \
      throw FatalError("Internal: " + std::string(msg)); \
    }                                                    \
  } while (false)

#endif // UTIL_H
