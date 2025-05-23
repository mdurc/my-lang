cmake_minimum_required(VERSION 3.16)

project(test_runner)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED True)

include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR}/../lexer
    ${CMAKE_CURRENT_SOURCE_DIR}/../parser
    ${CMAKE_CURRENT_SOURCE_DIR}/../logging
    ${CMAKE_CURRENT_SOURCE_DIR}/vendor
)


set(PARSER_SOURCES
    ../parser/parser.cpp
    ../parser/ast.cpp
    ../parser/symtab.cpp
    ../parser/visitor.cpp
    ../parser/types.cpp
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
    test_parser.cpp
    test_symtab.cpp
    ${LEXER_SOURCES}
    ${PARSER_SOURCES}
    ${LOGGING_SOURCES}
)

target_link_libraries(${PROJECT_NAME}
    gtest
    gtest_main
)

add_test(NAME ${PROJECT_NAME} COMMAND ${PROJECT_NAME})
