import ast
import sys

if len(sys.argv) > 1:
    filename = sys.argv[1]
    print(f"File name provided: {filename}")
else:
    print("No file name provided.")
    sys.exit()

with open(filename, "r") as file:
    tree = ast.parse(file.read())

module_doc = ast.get_docstring(tree)
print("Module Docstring:", module_doc)

for node in tree.body:
    if isinstance(node, ast.FunctionDef):
        print(f"Function '{node.name}' Docstring: {ast.get_docstring(node)}")
    if isinstance(node, ast.ClassDef):
        print(f"Class '{node.name}' Docstring: {ast.get_docstring(node)}")
