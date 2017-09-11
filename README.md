Zadání
======
Cílem je nakreslit obrázek podle zadaných nápověd. Na začátku každého řádku a
každého sloupce jsou čísla udávající počet a velikost souvislých vybarvených
bloků. Podle těchto nápověd je nutné určit (typicky jednoznační) výsledný
obrázek.

Uživatelská dokumentace
=======================
Nejprve se program zkompiluje
	`$ ghci griddlres.hs`
a spustí
	`$ ./griddlers`
Poté ze standardního vstupu čte zadání křížovky. Samotné zadání je v
následujícím formátu: Nejprve je na samostatném řádku číslo H, výška křížovky.
Za ní na dalším samostatném řádku je číslo W, šířka křížovky. Poté následuje H
řádků, každý řádek obsahuje nápovědy pro příslušný řádek, a to ve formě mezerou
oddělených čísel. Na konci je W nápověd pro sloupce, opět každá na samostatném
řádku ve formě mezerou oddělených čísel.

Výstupem programu je vyplněná křížovka. Znakem '#' jsou označena černě vyplněná
pole, znakem '.' bíle vyplněná pole. V případě když daná pravidla neumožňují
korektně vyplnit křížovku, křížovka se vůbec nevykreslí a uživatel je
informován o nekorektnosti zadání.

Programátorská dokumentace
==========================
Hlavní součástí programu jsou funkce, které daný řádek (resp. sloupec) vyplní
co nejlevější a co nejpravější možností, příslušné překrývající se bloky
stejných černých a bílých se určí jako příslušná barva. Opakovaně se tento
postup aplikuje na všechny řádky a sloupce. Ve většině případů je výsledkem
zcela vyplněná křížovka.

Pokud tento postup nepostačuje k úplnému vyplnění křížovky, program si "tipne"
první neznámé pole jako černé a zkusí doplnit křížovku. Buď nalezne korektní
vyplnění a nebo ví, že dané pole je bílé a pracuje s touto informací.
