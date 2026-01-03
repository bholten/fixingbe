# Historical Bio-Engineer Guide

This was a guide posted by a user circa 2003 to the SWG forums. It's posted here in its entirety. It has pertinent info to Bio-Engineering, but may reflect historical misunderstandings or community theories.

Source: https://www.swgemu.com/archive/scrapbookv51/data/20080108200838/be_guide.html

## Guide

BE pets are a balancing act with 5 DNA samples and up to 10 experimentation points. You begin with 5 DNA samples, which must be placed across 5 characteristics, physique, prowess, mental, psychology and aggression, to create a Template. Once the Template has been made you may spend your experimentation points as you wish on these 5 characteristics up to their maximums. In this document any predictions for values will be for the maximums. The initials values generally fall into the 60-80% of maximum range. It is not clear to me why they are some times closer to 60% and some times all in the 80-85% range. Once you have spent all of your experimentation points you get the final Template. Pick a "skin" and add into it the final Template plus meat and flora food to create a creature. It sounds fairly easy but none of the DNA samples contribute to just one thing and so it generally ends up a balancing action to get the creature you want. These 5 DNA samples will control HAM (Health/Action/Mind), armor, resists, damage, attack speed, chance to hit, ranged attack, 2 different special attacks and the creature level. The skin you pick will determine the creatures run speed, minimum creature level and whether its "agro" (i.e. could it ever be a non-CH pet and does it deathblow).

DNA Sampling

The profession does not come with a built in macro for doing DNA sampling. Bring up the macro page and add a new macro with /sampledna. There are two kinds if creatures you need to worry about agro and not. Creatures that are not agro can be approached without the scent mask skill (Scout exploration line). I'd still suggest having it up as approaching my still alarm them and cause the sample to be more difficult. Please note that ALL creatures can agro you while your sampling. Your first clue off this will be the creature hitting you. Its good to be aware of which creatures are social as nearby creatures of that type will also attack you if you attack back. It is important to know which deathblow as you do not want to risk being incapacitated by them. Make sure you burst run from any creature that DB's and that you have scouted an escape route. I'd suggest a route that leads right into a group of non-aggros. Agro creatures must be approached with the scent mask ability up. As the CL of the creature your approaching increases the more caution you need to take in the approach. The first thing to remember is that walking is better than running and crawling is better than walking for the approach. While sampling you are better off kneeling than standing. Really dangerous creatures should be approached from behind while wearing a suit of scent neutralizes clothing. (Please remember than +25 is supposed to be the max bonus you should be getting out of these clothes) I'd also suggest have a macro ready (that does /stand; /pause 1; /burstrun) in case your aggrod. Steep hills and other terrain can also help you get away without needing to waste your burst run. Some people also have luck approaching the creature by vehicle with scent masked. Then hitting a macro (It needs the full vehicle name, e.g. /mount speederbike to get away. If you're a ranger the camo kits can also be very useful. Alternately you can see if any of the MBE's on your server have a vendor where they sell DNA they have collected. However you're going to work make sure you have LOTS of inventory space. Make sure your wearing a backpack and get 3 droids with inventory space. (Preferably 10 spaces per droid). In total this should give you 140 inventory slots. You will find you'll need it because DNA does not stack and you need a lot of it. I'd also suggest bringing a cheap shield generator and muon gold as both can help make this easier.

Experimental Increases

There are 5 places to experiment. Physique controls fortitude and hardiness. Prowess controls endurance and dexterity. Mental controls intellect and cleverness. Psychology controls dependability and courage. Aggression controls fierceness and power. When you experiment on one of the slots the related attributes will increase. Please note that when you critically fail experimentation it is common to see one of the related attributes to go up, one to go down and one unrelated attribute will also go down. If you have already maxed out the slot related to this third attribute, you will not be given the option to experiment it back up. BE experimental failure seem to be as high as 20% with good crafting tools and good private crafting stations, so be careful. Please note that it is normal for 1 of the 2 attributes to max out during experimentation long before the other one. For instance I frequently see fortitude max out long before hardiness. The following system works fairly well to determine the expected increase for a success on experimentation on all of the slots except psychology. (It usually works for 1st generation psychology, but not always then)

For a physique experimentation point:-
Hardiness increase = [ F / ( F + H )] x 140
Fortitude increase = [ H / ( F + H )] x 140

HAM

Each of the HAM attributes (Health/Action/Mind) is affected by the inputted DNA samples. However the only way to affect Health positively in experimentation is to experiment on physique. The only way to effect Action positively is to experiment on prowess. The only way to affect Mind positively is to experiment on mental. The following show how the DNA slots affect the initial/final outcome.

Health is determined by hardiness. Hardiness is determined by the physique algorithm, physique 40%, prowess 25%, mental 5%, psychology 5% and aggression 25%. 500 will give you about 9000 health. (Final combine randomizes this a bit)

Action is determined by dexterity. Dexterity is determined by the prowess algorithm, physique 25%, prowess 42%, mental 17%, psychology 8.5% and aggression 7.5%. 500 will give you about 9000 action. (Final combine randomizes this a bit)

Mind is determined by intellect. Intellect is determined by the mental algorithm, physique 5%, prowess 10%, mental 50%, psychology 30% and aggression 5%. 500 will give you about 9000 mind. (Final combine randomizes this a bit)

As you can see each of these stats is effect to varying degrees by all 5 DNA samples entered into the samples. Of the three stats Health is usually considered the most important for a pet, action the second most.

Armor and Resists

Armor is determined by fortitude. If the fortitude is over 500 you get armor, under and you don't. Fortitude is determined as follows physique 40%, prowess 25%, mental 5%, psychology 5% and aggression 25%. To experiment fortitude up you put experiment points in physique. Please note that it is common for fortitude to max out before hardiness. Once fortitude stops moving upward on a successful experiment of physique then fortitude is maxed out.

Resists are a little more complicated. Resist are a little more complicated. To start with there are 8 resists, kinetic, energy, blast, heat, cold, electric, acid and stun. Each of these resists can come in 3 flavors effective, special and vulnerable. Vulnerable will show up as such on both the creature and the DNA samples. Effective and special resist can only be determined by examination by someone with sufficient creature-knowledge (scout and ranger ability). The stats for wild creatures can also be found on web sites such as http://www.swgcreatures.com.

The SWG official explanation for armor and resist can be found at http://starwarsgalaxies.station.sony.com/content.jsp?page=Advanced%20Guide%20Armor%20Fundamentals.

Basically effective and special resist damage in exactly the same way. They do not however work the same way with respect to the BE cloning process. Vulnerable acts as a -99 special resist. Resistance is determined by the standard physique determination algorithm, physique 40%, prowess 25%, mental 5%, psychology 5% and aggression 25%. However specials resists always override effective resist making the effective resist act as a 0 for the calculation. If the final resist is negative then the resist will be marked as vulnerable. Please note that effective resist will come through with part of its value. It will then require physique experimentation to bring it up to its full value. It cannot be brought over its final value by experimentation. It will also go back to 0, when fortitude goes over 500 and armor is achieved. Further experimentation will cause it to go up again. Additionally effective resists may be brought over their determined figure by a great result on the final combine. Special resists will always be at their determined amount and are unaffected by experimentation. Please note that kinetic and energy have a BE ceiling of 60 and the rest seem to have a ceiling of 100.

Examples of the resist calculation
physique/prowess/mental/psychological/aggression = resist
15E + 15E + 15E + 15E + 15E = 15*0.4 + 15*0.25 + 15*0.05 + 15*0.05 + 15*0.25 = 15E
15S + 15S + 15S + 15S + 15S = 15*0.4 + 15*0.25 + 15*0.05 + 15*0.05 + 15*0.25 = 15S
15S + 15S + 15E + 15E + 15E = 15*0.4 + 15*0.25 + 0 + 0 + 0 = 9.75 = 9S
15E + 15E + VLN + 15E + 15E = 0 + 0 + -99*0.05 + 0 + 0 = -4.95 = Vulnerable
15S + 15S + VLN + 15S + 15S = 15*0.4 + 15*0.25 + -99*0.05 + 15*0.05 + 15*0.25 = 9S

The system will not currently track effective vs. special resist for you so you will need to track it yourself. This means that it will not show up in the DNA sample. If you lack the Scout abilities to determine this then your best bet is to go to http://www.swgcreatures.com.

Damage, Attack Speed and To Hit

Damage is determined by power. Power uses the aggression algorithm, physique 17%, prowess 16%, mental 8.5%, psychology 16.5% and aggression 42%. You can multiply the power by 0.8 and round up to the next multiple of 10 to get a rough estimate of the lower damage limit. The upper damage limit is usually 10 points higher than the lower damage limit. (I have had both 55-65 damage and 70-75 damage, so these are just guide lines)

Attack speed is determined by courage. Courage uses the psychology algorithm, physique 9%, prowess 5%, mental 26%, psychology 43% and aggression 17%. The speed is usually close to 2.5 - (courage rounded up to the next 10)/1000.

ToHit is determined by cleverness. Cleverness uses the mental algorithm, physique 5%, prowess 10%, mental 50%, psychology 30% and aggression 5%. The ToHit is usually close to 0.19 + cleverness/1500.

Ranged Attack, Special1 and Special2

The mental DNA inserted determines ranged attack for me with a high degree of certainty. However several people have managed the same affect using the psychology slot. The DNA sample is question needs to have the ranged ability. The chance of transference then seems to be based on the quality of the DNA sample. I have reliably gotten ranged attack to come through when the only ranged DNA sample was in the mental slot and was of very high quality.

The general belief is that by preference special 1 comes from aggression and special 2 from psychology. Please note that they do not have to come from the same DNA slots. Multiple copies of an attack seem to cause a preference for that attack. Slots lacking specials seem to shift the specials off to other slots but do not make it definite that an attack will come through. Invalid specials (Area-Of-Effect such as plague strike, poison spray, open wounds) seem to express themselves as empty specials. By this I mean that if an invalid specials is picked for the special attack slot will come through as missing a special for that slot. This can mean you are likely to get no special for 1 or 2, as appropriate.

Creature Level

The short version is we don't really understand the algorithm that is used. The hope would be that CL would take all of the above into effect. Practically however it seems to mostly depend on damage, resists and the CL of the donors. In addition the CL determining algorithm is supposed to change soon. This makes most of our current guesses fairly pointless. Please note however there are a few important levels from a sales perspective. CL10 is the non-CH level. CL12 is the novice CH level. CL23/24 is the "triplets" level for an MCH. (23 + 23 + 24 = 70). CL35 is the "twins" level for an MCH. (35 + 35 = 70). CL 60-70 is the single large, hay look what I've got level. You also need to watch out for agro Templates when combined with CL 60+ creatures. To control these you need some form of aggressive bonus. The two standard places to get these bonuses are the Wookie racial bonus and taming bonuses from clothing. (The second comes from our tissue line)

Template Level

Templates have the same quality characteristics that DNA samples have. (Poor, below average, average, above average, high and very high) We assign numbers to them Poor = 0, below average = 1, average = 2, above average = 3, high = 4, and very high = 5. A DNA Template will have a quality, which is the average of its 5 samples rounded down. This means that it takes 5 very high quality samples to make a very high quality Template. It also mean a 1 above average, 3 high and 1 very high quality samples will make a high quality Template. There seems to be some advantage to using higher quality Templates when doing the final combine. It also seems to affect the type of DNA gotten for nth generation DNA samples.

Skins

Skins determine movement speed, aggression level and minimum CL for a creature. They are applied as the last part of the combine. Aggression level and minimum CL determines who can tame/use the pet. Non-CH must by 10 or under and not aggressive. Low-level CH's also tend to be unable to handle aggressive pets.

Aggressive pets include Angler, Boar Wolf, Bocatt, Choku, Huurton, Kusak, Langlatch, Shear Mite, Bordock, Dune Lizard, Narglatch, Woolamander, Kliknik, Vesp, Graul, Rancor and Kimogila.

CL 2 minimum Pets include Angler, Bearded Jax, Boar Wolf, Bocatt, Choku, Durni, Eopie, Gnort, Hermit Spider, Huurton, Kima, Krahbu, Kusak, Langlatch, Mott, Roba, Shear Mite, Slice Hound, Squall, Swirl Prong, and Vir Vur.

CL 5 minimum Pets include Bageraset, Bantha, Blurrg, Bol, Bolle Bol, Bolma, Bordok, Brackaset, Carrion Spat, Cu Pa, Dalyrake, Dewback, Dune Lizard, Falumpaset, Gualama, Guf Drolg, Gurnaset, Gurrcat, Gurreck, Ikopi, Kaadu, Kahmurra, Kwi, Mawgax, Narglatch, Pugoriss, Verne, and Zucca Boar.

CL 10 minimum Pets include Huf Dun, Piket, Razor Cat, Veermok, and Woolamander.

CL 15 minimum Pets include Gronda, Kliknik, Ronto, Snorbal, Thune, Tyblis and Vesp.

CL 20 minimum Pets include Malkloc and Torton.
CL 25 minimum Pets include Graul, Merek and Sharnaff.
CL 30 minimum Pet is the Fambaa.
CL 35 minimum Pet is the Rancor.
CL 40 minimum Pet is the Kimogila.

Currently the colors on some pets do not seem to stick once tamed. Kimogilas, Rancors and Jaxs are believed to have this problem. Razor Cat, Angler, and Kwi have all been confirmed to not have this problem. The problem will only manifest itself when the pet is tamed. As a statue the pet will show its color. On all pets the color seems to be absent in the datapad. In addition the examine function from the scout line seems to produce odd results.

Skin also seems to effect whether the ranged attack from the Template can actually be used. Apparently only skins, which have wild creature with ranged attack, can actually use ranged attack. Skins where the creature the skin is named for have ranged attack are, angler, bagaraset, blurrg, Bocatt, cu pa, Dune Lizard, kliknik, shear mite, Squall and Vesp. Skins which have ranged version but it is not the names creature are hermit spider (hermit spider queen), bolle bol (bolle bol colt), dalyrake (dalyrake matriarch) kwi (rhoa kwi guardian), merek (toxic merek battlelord) and kimogila (giant dune kimogila).

