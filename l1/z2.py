import os
import random

import sys

sys.setrecursionlimit(2000)


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
                    try:
                        yield [word] + dp(text[i + 1:])
                    except ValueError:
                        pass

        return max(step(), key=value)

    try:
        return dp(text)
    except ValueError as e:
        raise Exception('not enough words') from e


def load_words(filename='polish_words.txt'):
    with open(os.path.abspath(os.path.join(os.path.curdir, filename))) as f:
        return set(line.strip() for line in f)


def get_random_text(words, n):
    return ''.join(random.sample(words, n))


def show_some_results():
    l = [
        'tamatematykapustkinieznosi',
        'oprogramowywanąposznurowalibymentolinachoposowymiuszarganejnadstawianinienawykłauwarstwianonadsyłanychprzepiłowanego',
        'niezawieszańrozkiszeniomssakokształtnaprzekonsultujżehandlaromówcązaskorupianokonkatedralnąwykosztowywanieallosomowemubzikowatazagrzebmyniedwustronnemunieapozycyjnemueksplodującemuleżankoczyściusieńkiminiecelofanowymikomnatkapachciarscy',
        'cięłystajałeubiłamkeynesiściewydobrzonymipsychagogiokiszówidealistówzaładowałbygnuśniałyśmyniewybitymikropnijcieczubiłbyśodpluskwiłbyśprzymnażającegofotogrametrachmailowałeśnieprzecieknięćniepryszczykowesupermolekułęodświętnenadcioszżeceremoniowałeśkeczupowąwypraszającegowzniecajżekoabitowałopochlebiłabyśszpuntującymszargałabyrozwiązywałaśsmażżesystematyzujeszfajdajofiarodawcówniezamiedzonegozatargiemułożyskowałbyprzeobraźnieprzykuwanymannozyduużagleniomśrubokrętówbroiłydomontowananiepapugowaniomcierniowegodosypialibyściegrodeturowejnielaszowaniem'
        'aminokwasemniepozdawaniemświrekwybijałybyściegermaniezapomniałaniezgodomtaratatkamorowałbypozaorywaniamizżarliciupażkamiwpędzanynienormalnychzdrabnianiawazeliniarscynieszarżującenietrymowaniakarteluszkompodgiąłeśsztuczkomtamaryndusaminiemarionetkowąmorganitommyślałaśnieżwirowcowatązamojskiegossijmypocwałowałbyśnierozbolałychniepółjazzowiwkładajkatalpomniesmoktaniprzebodłybyściegranadyjskiejniewytykaniachlogarytmującewyreżyserujemypozjadalibyokrzesaniamiłobuzowatymdłutujmymorionowiwmontowaniachdziekaniomniepechowąniedyniowatymkiczowatościomścierajmyżposmarkajciechorionuwyżlarzaociekłbynienasycańzawirujżeniepowinowatenamoczyłybyśmybezokiennąnieturlanywężowcatrójmiastachokostnowymnieobjadającągłuchołazianinfamiliantachzaprotestowanizwierzaniepolifonicznejzłudniejszegoskreczującychstachanowskimzapluskałynienapiętymiczęstującychzarównampogdakiwaniembłogosławiłopozastawiałaolepieńnabuntowywanezadowalaniuwygracujmykatapleksjoetalonachchytrzejącymigazozolównieopryskanychbookerapozaosobistychzaparkowałybyspłaszczyłbymwykańczałabyśniewyśpiewanemuostiariuszwyciśniętegoniegibanąandrusowskiejmyzyjskichwodorokwasom',
    ]
    words = load_words()
    print('Words loaded. Start processing...')
    l2 = [main(text, words) for text in l]
    print('Done. Printing...')
    for sentence in l2:
        print(*sentence, sep=' ')


if __name__ == '__main__':
    show_some_results()
