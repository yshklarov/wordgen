# wordgen

Wordgen creates random words or names using Markov chains. It takes a list of existing words as input, and creates random words that appear similar to the ones given.

To run:

    $ cabal configure
    $ cabal build
    $ ./dist/build/wordgen/wordgen [-s <statesize>=4] [-n <numwords>=10] training-file

Note that data structures are currently very inefficient, so you may have to adjust the stack size.

# Issues and limitations

- It's very slow and eats a ton of memory.
- Word length is not taken into account: often words get way too long.

# Examples

The following examples were generated from the files in the `data/` directory, using statesize = 4 characters.

German cities:

    Muggardenhof
    Eichel
    Schwein
    Kaiserloh
    Bisch
    Prinz-Marxgrün
    Hildwesthal-Zell am Main
    Warnsteinasäge
    Großefehlbach
    Blöckelberg

Japanese cities:

    Nisemoto
    Kuja
    Hiranagu
    Umusa
    Shiri
    Matsu
    Izuo-kitanori
    Atsu
    Suehiro
    Nishifukubara

English words:

    brandesline
    volution
    wess
    baum
    jeepsilo
    timming
    droport
    disgrava
    endota
    glarientless

Russian words:

    ефрейтор
    средник
    центригорох
    высокока-коладка
    дурно
    переулок
    алладающий
    обдать
    примень
    размахиня

Klingon words:

    tIjwI'ghom
    ragh
    por
    ngIq
    mIllogh
    jIr
    chamwI'
    Sev
    Hochlogh
    Dugh

From Chapter I of Shelley's "Frankenstein", read in as a single "word" (ie. on a single line) with statesize = 8:

    I am by his abode, fair to Geneva and it necessary that he shores
    of then. There he hall of one of life than sister--their rude
    abode. Overjoyed at this evening paid his public functions with
    the schiavi ognor frementi, who seek him for her prospected my
    mother, was rapidly deplored to her. The inmate friend to the
    brighter the Reuss. But Caroline Beaufort possessed a lesson often
    months, and my mother, as an infant and at thered unknown and
    reputation of devoted affectual measure which all seemed to united
    that republic function; his rankling without looking as the
    chamois of his retreat in a merchant's house, while I shared it
    necessary that while I shared of a Milanese nobleman. He came a
    husband a being her love, and bloomed in the hills. The father of
    my pleasure for it was thin and syndics, and despair to her foster
    parents and was one which the wreck of her wind and it was clear
    and unbending me are my father rustic guardians to his fortunate
    circumstance. Beaufort, was a relating to the wreck of his public
    business that Elizabeth Lavenza became like a pretty presence for
    reflection from every mine to nursed in through his friend her
    wind and as mine--mine of my infant life I received as a means
    continued with her childish seriousness of the victim of my infant
    according her clothed cherish. [...]
