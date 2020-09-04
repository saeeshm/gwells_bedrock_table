# Name: Saeesh Mangwani
# Date: 2020-05-20
# Description: Creating a list of comments that can be used for testing the extractor functions

# ==== Loading libraries ====
# A family of fundamental packages that assist with the manipulation, tidying and visualization of data in R
library(tidyverse)
library(tidytext)
input <- '"yield:  3gpm. method of drilling = drilled"

water at time of drilling:.5gpm @ 42ft,.5gpm @ 133ft, 1 @ 164ft,.5gpm @ 257ft. casing: 10ft.

water approx flow:.25 @ 196ft,75-80 @ 257ft,.5 @ 264ft,.75 @ 283ft. casing: 86ft.

water: .25gpm-213ft,.25-230,.25-272,.75-283,100 cas 10ft

rotary. measurements from ground level. fractures: 67ft, 71ft, 81ft, 90ft. water approx flow: 1.5gpm @ 67ft, 1.5gpm @ 72ft, 17gpm - 90ft. casong: 20ft. pumping should be no more than 3gpm.

p.l. 245ft @ 2gpm. method of drilling = drilled

.75gpm at 65ft.  9.25gpm at 185ft. method of drilling = drilled
rocks and gravel to 15ft. volcanic rock with stringers of granite @ 60 and 80ft with water @ 97ft (3gpm). increasing to 9gpm @ 107ft.approx. 11gpm @ 120ft total depth: 150ft

yield:  6gpm 1 @ 10ft/ 2 @ 16ft/ 3 @ 50ft

4ft. of casing (6") 1ft. in solid rock.25gpm at 20ft. sandstone to 50ft.5gpm at 60ft in traces of quartz more water at 80ft 115ft 2gpm in layer of quartz 120ft 3gpm in quartz 30 feet of reservoir

yield: 90gph 1.5gpm @ 53ft method of drilling = drilled

trickle @ 3ft, trace @ 10ft, 3gpm @ 1.5ft

@ 48ft - .25gpm, @ 120ft - 2gpm method of drilling = drilled

fractured at 135, 142, 193ft. - no water.  fractures 300-350ft. traced of water.  2gpm at 365ft.  increased to 6gpm from fractures at 370-375ft. method of drilling = drilled

fracture @ 135ft, 4gpm

.25gpm at 12 to 100ft. .25gpm at 100 to 288ft.  40gpm at 288 to 365ft. pump test and chem results inside well card. method of drilling = drilled

45ft--1.5gpm 135ft--2.5gpm method of drilling = drilled

11ft casing,frac @ 70ft 3gpm,100ft increases to 8gpm,150ft 15gpm,185ft 30gpm

frac @ 250, 300gpm, 40ft, 500gpm

rocks and gravel to 15FT. volcanic rock with stringers of granite @ 60 and 80ft with water @ 97ft (3GPM). increasing to 9gpm @ 107ft.approx. 11gpm @ 120ft total depth: 150ft

fracture @ 50, 45, 50ft

fractures lie at 50ft, 35ft

granite water @ 39ft, 50ft, 53ft.

another well drilled on the same property was a dry hole also in rock, mostly very hard wark gneiss. water sources in the rock are at 34ft and 90ft.yield 20gph method of drilling = drilled

sources:  2gpm @ 15ft.  2gpm @ 26ft.  yield: 5gpm @ 25ft. very hard water. method of drilling = drilled

soil and broken rock to 3.5ft metamorphise rock to 160FT granite with fractures @ 210ft - 245ft and a trace of water @ 275ft. water increase to @ 350ft 420ft good stream of water 4gpm

frac @ 250, 300gpm, 40ft, 500gpm

frac @ 2ft, 5ft, 8ft

rocks and gravel to 15FT. volcanic rock with stringers of granite @ 60 and 80ft with water @ 97ft (3GPM). increasing to 9gpm @ 107ft.approx. 11gpm @ 120ft total depth: 150FT

12 to 100ft at .5gpm. 100 to 288ft at .25gpm'

# Splitting the input by newline
in_split <- str_split(input, "\\n")[[1]]
# Removing empty strings
testing <- in_split[!in_split == ""]
# Cleaining unnecessary characters. Done!
testing <- str_remove_all(testing, '"')
