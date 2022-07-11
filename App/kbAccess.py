from pyswip import Prolog
import os
import posixpath


class PrologInterface(object):
    def __init__(self, prolog_path='../prolog', prolog_extension='.pl'):
        self.prolog = Prolog()
        self.prolog_path = prolog_path
        self.prolog_extension = prolog_extension
        self.to_consult = os.listdir(posixpath.join('', self.prolog_path))
        self.to_consult = list(filter(lambda x: x.endswith(self.prolog_extension), self.to_consult))
        self.true = [{}]
        self.false = []

    def load_rules(self):
        for pl in self.to_consult:
            self.prolog.consult(posixpath.join('', self.prolog_path, pl))

    def query(self, query):
        return list(self.prolog.query(query))

    def get_prolog_path(self):
        return self.prolog_path

    def get_prolog_extension(self):
        return self.prolog_extension

    def get_prolog_files(self):
        return self.to_consult

    def set_to_consult(self, files):
        self.to_consult = files


