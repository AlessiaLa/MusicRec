from pyswip import Prolog

prolog = Prolog()

prolog.consult("C:/Users/Alessia/Documents/GitHub/MusicNet/prolog/album.pl")

c = bool(list(prolog.query("album(X,\"cleopatra\")")))
print(c)

