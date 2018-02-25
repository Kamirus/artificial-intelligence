import os
import random

import sys

sys.setrecursionlimit(5000)


def open_relative(filename, *args, **kwargs):
    return open(os.path.abspath(os.path.join(os.path.curdir, filename)), *args, **kwargs)


def value(word_list):
    return sum(len(word) ** 2 for word in word_list)


def main(text, words):
    def memo(f):
        m = {}

        def aux(x):
            if x not in m:
                m[x] = f(x)
            return m[x]

        return aux

    @memo
    def dp(text):
        if not text:
            return []

        def step():
            for i, _ in enumerate(text):
                word = text[0:i + 1]
                if word in words:
                    sub = dp(text[i + 1:])
                    if sub is not None:
                        yield [word] + sub

        return max(step(), key=value, default=None)

    res = dp(text)
    assert res is not None, 'not enough words'
    return res

def load_words(filename='polish_words.txt'):
    with open_relative(filename) as f:
        return set(line.strip() for line in f)


def get_random_text(words, n):
    return ''.join(random.sample(words, n))


def show_some_results(words):
    l = [
        'tamatematykapustkinieznosi',
        'oprogramowywanąposznurowalibymentolinachoposowymiuszarganejnadstawianinienawykłauwarstwianonadsyłanychprzepiłowanego',
        'niezawieszańrozkiszeniomssakokształtnaprzekonsultujżehandlaromówcązaskorupianokonkatedralnąwykosztowywanieallosomowemubzikowatazagrzebmyniedwustronnemunieapozycyjnemueksplodującemuleżankoczyściusieńkiminiecelofanowymikomnatkapachciarscy',
        'cięłystajałeubiłamkeynesiściewydobrzonymipsychagogiokiszówidealistówzaładowałbygnuśniałyśmyniewybitymikropnijcieczubiłbyśodpluskwiłbyśprzymnażającegofotogrametrachmailowałeśnieprzecieknięćniepryszczykowesupermolekułęodświętnenadcioszżeceremoniowałeśkeczupowąwypraszającegowzniecajżekoabitowałopochlebiłabyśszpuntującymszargałabyrozwiązywałaśsmażżesystematyzujeszfajdajofiarodawcówniezamiedzonegozatargiemułożyskowałbyprzeobraźnieprzykuwanymannozyduużagleniomśrubokrętówbroiłydomontowananiepapugowaniomcierniowegodosypialibyściegrodeturowejnielaszowaniem'
        'aminokwasemniepozdawaniemświrekwybijałybyściegermaniezapomniałaniezgodomtaratatkamorowałbypozaorywaniamizżarliciupażkamiwpędzanynienormalnychzdrabnianiawazeliniarscynieszarżującenietrymowaniakarteluszkompodgiąłeśsztuczkomtamaryndusaminiemarionetkowąmorganitommyślałaśnieżwirowcowatązamojskiegossijmypocwałowałbyśnierozbolałychniepółjazzowiwkładajkatalpomniesmoktaniprzebodłybyściegranadyjskiejniewytykaniachlogarytmującewyreżyserujemypozjadalibyokrzesaniamiłobuzowatymdłutujmymorionowiwmontowaniachdziekaniomniepechowąniedyniowatymkiczowatościomścierajmyżposmarkajciechorionuwyżlarzaociekłbynienasycańzawirujżeniepowinowatenamoczyłybyśmybezokiennąnieturlanywężowcatrójmiastachokostnowymnieobjadającągłuchołazianinfamiliantachzaprotestowanizwierzaniepolifonicznejzłudniejszegoskreczującychstachanowskimzapluskałynienapiętymiczęstującychzarównampogdakiwaniembłogosławiłopozastawiałaolepieńnabuntowywanezadowalaniuwygracujmykatapleksjoetalonachchytrzejącymigazozolównieopryskanychbookerapozaosobistychzaparkowałybyspłaszczyłbymwykańczałabyśniewyśpiewanemuostiariuszwyciśniętegoniegibanąandrusowskiejmyzyjskichwodorokwasom',
    ]
    l2 = [main(text, words) for text in l]
    print('Done. Printing...')
    for sentence in l2:
        print(*sentence)


def pan_tadeusz(words):
    with open_relative('pan_tadeusz_bez_spacji.txt') as f:
        for l in f:
            print(*main(l.strip(), words))


if __name__ == '__main__':
    words = load_words()
    print('Words loaded. Start processing...')
    # show_some_results(words)
    pan_tadeusz(words)
