import ast
import sys

if len(sys.argv) > 2:
    filename = sys.argv[1]
    outfilename = sys.argv[2]
    print(f"# File names provided: {filename} {outfilename}")
else:
    print("No file name provided.")
    sys.exit()

with open(filename, 'r') as file:
    source_code = file.read()

tree = ast.parse(source_code)
with open(outfilename, "w", encoding="utf-8") as file:
    file.write(ast.dump(tree, indent=4))
