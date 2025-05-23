#define APPROVALS_GOOGLETEST_EXISTING_MAIN
#include "vendor/ApprovalTests.hpp"

std::string rtrim(const std::string& s) {
  size_t end = s.size();
  while (end > 0 && std::isspace(static_cast<unsigned char>(s[end - 1]))) {
    --end;
  }
  return s.substr(0, end);
}

int main(int argc, char** argv) {
  ::testing::InitGoogleTest(&argc, argv);
  ApprovalTests::initializeApprovalTestsForGoogleTests();

  auto directoryDisposer =
      ApprovalTests::Approvals::useApprovalsSubdirectory("snapshots");

  auto reporter = ApprovalTests::CustomReporter::createForegroundReporter(
      "nvim", "-d {Received} {Approved}");

  ApprovalTests::DefaultReporterDisposer defaultReporterDisposer =
      ApprovalTests::Approvals::useAsDefaultReporter(reporter);

  return RUN_ALL_TESTS();
}
