#ifndef JSON_EXPORT_JSON_EXPORTER_H
#define JSON_EXPORT_JSON_EXPORTER_H

#include <memory>
#include <string>
#include <vector>

#include "../logging/logger.h"
#include "../parser/ast.h"
#include "../parser/symtab.h"

class JsonExporter {
public:
  JsonExporter(const SymTab* symtab, const Logger* logger,
               const std::vector<AstPtr>* ast_nodes)
      : m_symtab(symtab), m_logger(logger), m_ast_nodes(ast_nodes) {}

  std::string export_to_json();

private:
  const SymTab* m_symtab;
  const Logger* m_logger;
  const std::vector<AstPtr>* m_ast_nodes;
};

#endif // JSON_EXPORT_JSON_EXPORTER_H
