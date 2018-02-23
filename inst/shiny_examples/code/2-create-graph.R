## Generation of a exploratory graphs

## Visual exploration of household will provide a quick overview

#########################################################################################
## Produce graphs of all select_one questions
kobo_bar_one()

#########################################################################################
## Produce graphs of all select_multiple questions
kobo_bar_multi()


#########################################################################################
## Produce histogramme for all numeric variable
kobo_histo()



########################################################################################
### Produce faceted chart select_one

kobo_bar_one_facet()

########################################################################################
### Produce correlation

kobo_correlation()


########################################################################################
### Produce boxplot
kobo_boxplot_facet(household,  dico)
