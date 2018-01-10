
if(!exists('alpha')){
load('Alpha.rda')
load('N.rda')
load('thetavFC.rda')
load('kappaFC.rda')
load('thetavSS.rda')
load('kappaSS.rda')
source('filterAndSpectrumParameters.R')
source('000_H.R')}
#rev all var for re-organizing text
alpha <- rev(round(alpha*1e2,2))
alphase <- rev(round(alphase*1e2, 2))
N <- rev(round(N*1e3,2))
Nse <- rev(round(Nse*1e3, 2))
f <- rev(round(f*1e4,2)[1:2])
H <- rev(round(H)[1:2])
thetav <- rev(round(c(mean(thetavFC), mean(thetavSS)),2))
thetavse <- rev(round(c(sd(thetavFC)/sqrt(length(thetavFC)),
				sd(thetavSS)/sqrt(length(thetavSS))),2))
thetavn <- rev(c(length(thetavFC), length(thetavSS)))
kappa <- rev(round(c(kappauFC*1e4, kappauSS*1e4),2))
kappase <- rev(round(c(kappauseFC*1e4, kappauseSS*1e4),2))
kappan <- rev(c(kappaunFC, kappaunSS))

header <- c('Mooring', '$\\alpha$ \\, \\, \\, \\, ($\\times 10^{-2}$)',
			'$N$ ($\\times 10^{-3} \\text{s}^{-1}$)',
			'$f$ ($\\times 10^{-4} \\text{s}^{-1}$)',
			'$H$ (m)',
			'$\\theta_v$ \\, \\, \\, \\,(degrees)',
			'$\\kappa_H$ ($\\times 10^{-4} \\text{m}^{-1}$)')
			
#mooring <- c('FC', 'SS')
mooring <- c('SS', 'FC') #for re-ordered table
talpha <- paste(alpha, ' (SE=', alphase,',  df=',alphan-1,')',sep="")
tN <- paste(N ,' (',Nse,', ',Nn,')', sep="")
tthetav <- paste(thetav, ' (', thetavse,', ', thetavn,')', sep="")
tkappa <- paste(kappa, ' (', kappase,', ', kappan,')',sep="")

table <- cbind(mooring, talpha, tN, f, H, tthetav, tkappa)
colnames(table) <- header
caption <- 'Inferred parameters, with standard errors and degrees of freedom, for the test of the Rossby wave dispersion relation.'
label <- 't:background'
align <- c('p{.5in}','p{.5in} |','p{.7in}', rep('p{.58in}',2), 'p{.5in}', rep('p{.58in}',2))

