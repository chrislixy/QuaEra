table(purchase$A, purchase$State)
table(purchase$B, purchase$State)
# No one bought C=1 in GA and ME
table(purchase$C, purchase$State)
# No one bought D=1 in GA
table(purchase$D, purchase$State)
table(purchase$E, purchase$State)
table(purchase$F, purchase$State)
# In ND and SD, G=2 was the only option
# In FL, people can choose between G=3 and G=4, G=3 is more likely
# No one bought G=0 in OH
table(purchase$G, purchase$State)

mosaicplot(table(purchase$A,purchase$B))
mosaicplot(table(purchase$A,purchase$C))
mosaicplot(table(purchase$A,purchase$D))
mosaicplot(table(purchase$A,purchase$E))
mosaicplot(table(purchase$A,purchase$F))
mosaicplot(table(purchase$A,purchase$G))

mosaicplot(table(purchase$B,purchase$C))
mosaicplot(table(purchase$B,purchase$D))
# BE pair B0E0,B1E1
mosaicplot(table(purchase$B,purchase$E))
mosaicplot(table(purchase$B,purchase$F))
mosaicplot(table(purchase$B,purchase$G))

#higher C--higher D
mosaicplot(table(purchase$C,purchase$D))
# higher C--higher chance D=1
mosaicplot(table(purchase$C,purchase$E))
mosaicplot(table(purchase$C,purchase$F))
mosaicplot(table(purchase$C,purchase$G))

# higher D -- higher chance E=1
mosaicplot(table(purchase$D,purchase$E))
mosaicplot(table(purchase$D,purchase$F))
mosaicplot(table(purchase$D,purchase$G))

mosaicplot(table(purchase$E,purchase$F))
mosaicplot(table(purchase$E,purchase$G))

mosaicplot(table(purchase$F,purchase$G))


# Compare first view, 2nd last view, and last view with purchase
table(firstview$A,purchase$A)
table(lastview$A,purchase$A)
mosaicplot(table(firstview$A,purchase$A))
mosaicplot(table(lastview$A,purchase$A))
mosaicplot(table(secondlastview$A,purchase$A))

mosaicplot(table(firstview$B,purchase$B))
mosaicplot(table(lastview$B,purchase$B))
mosaicplot(table(secondlastview$B,purchase$B))

mosaicplot(table(firstview$C,purchase$C))
mosaicplot(table(lastview$C,purchase$C))
mosaicplot(table(secondlastview$C,purchase$C))

mosaicplot(table(firstview$D,purchase$D))
mosaicplot(table(lastview$D,purchase$D))
mosaicplot(table(secondlastview$D,purchase$D))

mosaicplot(table(firstview$E,purchase$E))
mosaicplot(table(lastview$E,purchase$E))
mosaicplot(table(secondlastview$E,purchase$E))

mosaicplot(table(firstview$F,purchase$F))
mosaicplot(table(lastview$F,purchase$F))
mosaicplot(table(secondlastview$F,purchase$F))

mosaicplot(table(firstview$G,purchase$G))
mosaicplot(table(lastview$G,purchase$G))
mosaicplot(table(secondlastview$G,purchase$G))
