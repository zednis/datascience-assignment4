import json
import click
import requests
import os.path
from clint.textui import progress


class Downloader:
    def download(self, source, dest, size):
        r = requests.get(source, stream=True)

        # ensure data/ directory exists
        d = os.path.dirname("data/")
        if not os.path.exists(d):
            os.makedirs(d)

        with open("data/" + dest, 'wb') as f:
            print("Downloading", dest)
            for chunk in progress.bar(r.iter_content(chunk_size=1024), expected_size=(size / 1024) + 1):
                if chunk:
                    f.write(chunk)
                    f.flush()

    def checksum(self, checker):
        for mapping in self.mappings:
            assert checker("data/" + mapping["file"]) == mapping["checksum"]

    def __init__(self, mappings):
        self.mappings = mappings
        for mapping in self.mappings:
            if not os.path.isfile('data/' + mapping["file"]):
                self.download(mapping["source"], mapping["file"], int(mapping["size"]))
