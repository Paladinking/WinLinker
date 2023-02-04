#!/usr/bin/python3
import pdftotext

page_index = range(3, 8)

pages = range(114, 404)

header_info = {"[AMD Public Use]", "Contents", "\x0c", "AMD64 Technology", "24594—Rev. 3.34—October 2022", "ii", "iii", "iv", "v", "vi"}

first_line = "General-Purpose Instruction Reference . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .73"
last_line = "4"

def valid_row(row):
    return row and not row in header_info

def main():
    with open("24594.pdf", 'rb') as file:
        pdf = pdftotext.PDF(file)
    
    lines = []
    for page in page_index:
        for row in filter(valid_row, pdf[page].split('\n')):
            lines.append(row)

    first = lines.index(first_line) + 1
    last = lines.index(last_line)
    
    instructions_data = []
    i = first
    while i < last:
        name = []
        parts = lines[i].split(' .')
        while len(parts) == 1:
            i += 1
            name.append(parts[0])
            parts = lines[i].split(' .')
        name.append(parts[0])
        pos = int(parts[-1]) - 75 + 114
        instructions_data.append((name, pos))
        i += 1
        
    instructions = []
    i = 0
    while True:
        name = []
        while instructions_data[i][1] == instructions_data[i + 1][1]:
            i += 1
        if i == len(instructions_data) - 1:
            break
        i += 1
        

    for instruction in instructions_data:
        print(instruction)


if __name__ == "__main__":
    main()