#ifndef TEST_HARNESS_H
#define TEST_HARNESS_H

#include <string>
#include <vector>

namespace Testing {
bool run_snapshot_test(const std::string& test_name,
                       const std::string& input_filepath,
                       const std::string& snapshot_dir, bool update_snapshots);
} // namespace Testing

#endif
