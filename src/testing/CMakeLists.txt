cmake_minimum_required(VERSION 3.16)

project(test_runner)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED True)

include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR}/../lexer
    ${CMAKE_CURRENT_SOURCE_DIR}/../logging
    ${CMAKE_CURRENT_SOURCE_DIR}/vendor
)

set(LEXER_SOURCES
    ../lexer/lexer.cpp
    ../lexer/token.cpp
)

set(LOGGING_SOURCES
    ../logging/diagnostic.cpp
)

add_executable(${PROJECT_NAME}
    main.cpp
    test_lexer.cpp
    ${LEXER_SOURCES}
    ${LOGGING_SOURCES}
)

target_link_libraries(${PROJECT_NAME}
    gtest
    gtest_main
)

add_test(NAME ${PROJECT_NAME} COMMAND ${PROJECT_NAME})
