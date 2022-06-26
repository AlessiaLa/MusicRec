import os

from pyswip import Prolog
os.chdir(os.path.join(os.getcwd(), "prolog"))


path = os.path.join(os.getcwd(), "album.pl").replace("\\","/")
print(path)
prolog = Prolog()

prolog.consult(path)

c = bool(list(prolog.query("album(X,\"cleopatra\")")))
print(c)

