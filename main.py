import json

from classes.convert import Converter
from classes.download import Downloader
from helpers.checksum import md5

mapping = json.load(open('mapping.json'))

d = Downloader(mapping["mappings"])
try:
	d.checksum(md5)
except:
	print "Checksum differs from saved version. You may need to download a back up"

c = Converter(mapping)

print "Done :)"