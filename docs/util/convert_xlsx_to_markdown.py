import pandas as pd
import sys


if __name__ == '__main__':
    args = sys.argv[1:]
    if not args or args[0].split('.')[-1] != 'xlsx':
        print('Point me to an excel file')

    file = args[0]

    table = pd.read_excel(file)

    md_table = table.to_markdown(index=False)

    file_stem = file.split('.')[0]
    with open(file_stem + '.md', 'w') as file:
        file.write(md_table)

