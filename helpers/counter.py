import pandas
import argparse

parser = argparse.ArgumentParser(description='Value count for columns.')
parser.add_argument('file', metavar='F', help='source file')
parser.add_argument('col', metavar='C', help='column to count values')
args = parser.parse_args()

data = pandas.read_csv(args.file)
print(data[args.col].value_counts())
