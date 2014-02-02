# wordgen

Wordgen creates random words or names using Markov chains. It takes a list of existing words as input, and creates random words that appear similar to the ones given.

To run:

1. 'cabal configure'
2. 'cabal build'
3. './dist/build/wordgen/wordgen'

Note that data structures are currently very inefficient, so you may have to adjust the stack size (to as high as 200M for the English dictionary.)

# Issues and limitations

- Command line options are not supported, so too many things are hard-coded.
- It's very slow and eats a ton of memory.
- Word length is not taken into account: often words get way too long.
- There is no way to adjust the algorithm dynamically, eg. keep four characters in the state instead of three.

# Examples

The following examples were generated using the files in the `data/` directory.

German cities:

    Klein Wolfsbrockbrach
    Bulandern
    Pönig
    Heckin-Bad Heide
    Wurlochshöhe
    Mettelsbrunn
    Ersfeld
    Schow
    Kienbach
    Blöckig

Japanese cities:

    Shirakiharuzawa
    Nakozan
    Kamikutsu
    Ginzan
    Toki
    Oyamachi
    Kawashi
    Itakura
    Yusanagamatomiyama
    Shimoara
    Nagatanohamanochi
    Kashimashi

English words:

    erreinvers
    offstartuplians
    unoflux
    excludermultificide
    premaine
    vendochrome
    exped
    pent
    whitmentic
    embrogryptimo

Russian words:

    ноженными
    идусь
    кулерато
    накропомнилась
    ого
    оглашаешь
    приками
    подполопамяточатленном
    разонном
    сопанные

Klingon words:

    tlhuQ
    rojmeH tIq
    per yuD
    ngan
    moghmoHwI'
    jach
    chetwI'
    bochHomwI'
    QeD
    DuSaQ
