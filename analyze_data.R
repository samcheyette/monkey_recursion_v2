library(ggplot2)
library(reshape)
library(grid)
library(dplyr)


t1 <- 	theme(axis.text=element_text(size=10), 
		strip.text.x = element_text(size = 8),
		axis.text.x=element_text(size=9),
		axis.title.x=element_blank(),
		axis.text.y=element_text(size=7),

		legend.title=element_blank(),
		legend.text=element_text(size=10),
		 legend.key.size = unit(4, 'lines')) 

t2 <- theme(axis.text=element_text(size=8), 
		strip.text.x = element_text(size = 4),
		axis.text.x=element_text(size=6),
		axis.title.x=element_blank(),
		axis.text.y=element_text(size=7),

		legend.title=element_blank(),
		legend.text=element_text(size=6),
		 legend.key.size = unit(4, 'lines'))

t3 <- theme(axis.text=element_text(size=13), 
		#strip.text.x=element_blank(),
		strip.text.x = element_text(size = 14),
		plot.title=element_text(size=18),
		axis.text.x=element_blank(),
		axis.title.x=element_blank(),
		axis.text.y=element_text(size=12),

		legend.title=element_blank(),
		legend.text=element_text(size=10),
		 legend.key.size = unit(4, 'lines'))

t4 <- theme(axis.text=element_text(size=18), 
		#strip.text.x=element_blank(),
		strip.text.x = element_text(size = 14),
		axis.text.x=element_text(size=17),
		#axis.title.x=element_text(size=14),
		axis.title.x=element_blank(),
		axis.text.y=element_text(size=17),

		legend.title=element_blank(),
		legend.text=element_text(size=16),
		 legend.key.size = unit(4, 'lines'))

data = read.csv("output/all_data6.csv")

m = melt(data, id=c("Who", "Participant","HorM",
					 "which", "var", "P"))

m$Participant <- factor(m$Participant)



###############################################
###################DIST########################

m.dist <- m %>% filter(grepl("distr", which)) %>% 
			mutate(var=gsub( "W", "(", var)) %>%
			mutate(var=gsub( "X", "[", var)) %>%
			mutate(var=gsub( "Y", "]", var)) %>%
			mutate(var=gsub( "Z", ")", var)) 



thresh <- 0.1





m.dist <- cbind(m.dist,
				rand=runif(nrow(m.dist), -0.00001,0.00001))

m.dist <- m.dist %>% 
		mutate(P2=(P * (grepl("hu",
			 as.character(HorM))) + rand))




m.dist <- m.dist %>% group_by(Participant, var) %>%
					mutate(psum=sum(P2)) 

					 #%>% 
					#filter(psum > thresh)

m.dist <- m.dist %>% group_by(Participant) %>%

					top_n(6, wt=psum)
head(m.dist)

p.dist.1 <- ggplot(m.dist, aes(x=var, y=P, 
				group=HorM)) +
			geom_bar(stat='identity',position='dodge',
				aes(group=HorM, fill=HorM)) +
			geom_text(aes(label=Who, x=0.5,y=0.9)) +
			facet_grid(Who~Participant,
						scales="free")

p.dist.1 <- p.dist.1 + 
				theme(axis.text.x =
				 element_text(angle = 90, hjust = -1)) +
				t1


ggsave("output/dist1.pdf", width=10, height=7)

##################################################
#######################TRACE######################


m.trace.1 <- m %>% filter(grepl("trace", which))%>% 
			filter(grepl("model", HorM)) %>% 
			mutate(var=gsub("W", "(", var)) %>%
			mutate(var=gsub("X", "[", var)) %>%
			mutate(var=gsub("Y", "]", var)) %>%
			mutate(var=gsub("Z", ")", var)) %>%
			mutate(var=gsub(" ", "", var))


m.trace.1 <- cbind(m.trace.1,
				rand=runif(nrow(m.trace.1), 0,1))


thresh <- 0.05

m.trace.1 <- m.trace.1 %>% group_by(Participant, var) %>%
					mutate(psum=(sum(P) + rand/10000)) %>% 
					ungroup #%>% 
					

m.trace.1 <- m.trace.1 %>% group_by(Participant) %>%
					top_n(n=4,wt=psum)
					#filter(psum > thresh)

p.trace.1 <- ggplot(m.trace.1, aes(x=var, y=P)) +
			geom_bar(stat='identity',position='dodge') +
			facet_wrap(~Who~Participant, scales="free")

p.trace.1 <- p.trace.1 + 
				#theme(axis.text.x =
				# element_text(angle = 90, hjust = -1)) +
				t2

ggsave("output/trace1.pdf", width=10, height=7)



#################################################
#######################TRACE2####################

pz <- function(x, y) {
	if (grepl("model",as.character(x))) {
		return(y)

	} else {
		return(0.0)
	}
}

p_what <- function(x, y, what) {
	if (grepl(what,as.character(x))) {
		return(y)

	} else {
		return(0.0)
	}
}

#head(m.trace.2)


for (i in 1:3) {
subs <- levels(m$Who)[i]
m.trace.2 <- m %>% filter(grepl("trace", which))

m.trace.2 <- m.trace.2 %>% filter(grepl(subs, 
			as.character(Who)))


m.trace.2 <- m.trace.2 %>%
			group_by(Participant, var, HorM) %>%
			mutate(Prp=pz(HorM, P)) %>%
			mutate(CE_frac=p_what(HorM, P, "CE")) 

m.trace.2 <- m.trace.2 %>%
			group_by(Participant, var) %>%
			mutate(Prp=sum(Prp)) %>%
			mutate(ProdP=Prp*P) %>%
			mutate(sumProdP=sum(ProdP)) %>% 
			mutate(CE_frac=CE_frac*Prp)
			ungroup

m.trace.2 <- m.trace.2 %>%
			group_by(Participant) %>%

			mutate(CE_tot=-sum(CE_frac))


m.trace.2 <- m.trace.2 %>% 
			ungroup %>%
			filter(!grepl("model", HorM)) %>% 
			mutate(var=gsub("W", "(", var)) %>%
			mutate(var=gsub("X", "[", var)) %>%
			mutate(var=gsub("Y", "]", var)) %>%
			mutate(var=gsub("Z", ")", var)) %>%
			mutate(var=gsub(" ", "", var))


m.trace.2 <- m.trace.2 %>% mutate(WhoPart=paste(Who, Participant))



m.trace.2 <- m.trace.2 %>% 
		mutate(HorM=(factor(gsub("OT", 
			"OTHER", as.character(HorM))))) %>%
		mutate(HorM=(factor(gsub("CE", 
			"CENTER-EMBEDDING", as.character(HorM))))) %>%
		mutate(HorM=(factor(gsub("CR", 
			"CROSSING", as.character(HorM))))) %>%
		mutate(HorM=(factor(gsub("TR", 
			"TAIL-RECURSION", as.character(HorM))))) 

m.trace.2 <- m.trace.2 %>% 
					group_by(Participant, HorM) %>% #%>%
					
					top_n(n=3,wt=Prp) %>% 
					print(1) %>%
					ungroup


					#filter(ProdP > thresh)

#m.trace.2$var <- as.factor(m.trace.2$var)
#m.trace.2$WhoPart <- as.factor(m.trace.2$WhoPart)


m.trace.2 <- m.trace.2 %>% #filter(grepl("CE", HorM)) %>%
			mutate(var2=paste(var, as.character(as.numeric(sumProdP)))) %>%
			transform(var2 = factor(reorder(var2, -sumProdP))) %>%
			transform(Participant = factor(reorder(Participant, 
										CE_tot)))

#m.trace.2 <- m.trace.2 %>% group_by()
			#mutate(rank=dense_rank())


m.trace.2$P <- signif(m.trace.2$P, 1)

p.trace.2 <- ggplot(m.trace.2, aes(x=var2, y=P)) +
			geom_bar(stat='identity',position='dodge', 
				aes(group=HorM, fill=HorM)) +
			geom_point(aes(x=var2,y=Prp), size=2.0) +

			geom_text(aes(label=var,x=var2,y=Prp),
				 size=5.0, vjust=1.0) +
			facet_wrap(~Participant, 
					scales="free_x", ncol=6) 


p.trace.2 <- p.trace.2 + 
				t3 + ggtitle(paste(subs, "Strategies and Their Properties")) +
				ylab("Probability")
				
				#+ title(subs) + xlab("Probability")

out <- paste("output/trace2", subs)	
out <- gsub(" ", "", paste(out, ".pdf"))


   #+
				#scale_x_discrete(label=abbreviate) 
ggsave(out, width=16, height=7)





}

#################################################
###################TRACE3########################

m.trace.3 <- m %>% filter(grepl("trace", which))


m.trace.3 <- m.trace.3 %>%
			group_by(Participant, var, HorM) %>%
			mutate(Prp=pz(HorM, P)) %>%
			mutate(CE_frac=p_what(HorM, P, "CE")) %>%
			mutate(CR_frac=p_what(HorM, P, "CR")) 


m.trace.3 <- m.trace.3 %>%
			group_by(Participant, var) %>%
			mutate(Prp=sum(Prp)) %>%
			mutate(ProdP=Prp*P) %>%
			mutate(sumProdP=sum(ProdP)) %>% ungroup

m.trace.3 <- m.trace.3 %>%
			group_by(Participant, var) %>%

			mutate(CE_tot=sum(CE_frac)) %>%
			mutate(CR_tot=sum(CR_frac))


m.trace.3 <- m.trace.3 %>% 
			ungroup %>%
			filter(grepl("model", HorM)) %>% 
			mutate(var=gsub("W", "(", var)) %>%
			mutate(var=gsub("X", "[", var)) %>%
			mutate(var=gsub("Y", "]", var)) %>%
			mutate(var=gsub("Z", ")", var)) %>%
			mutate(var=gsub(" ", "", var))


m.trace.3 <- m.trace.3 %>% mutate(WhoPart=paste(Who, Participant))



m.trace.3 <- m.trace.3 %>% 
		mutate(HorM=(factor(gsub("OT", 
			"OTHER", as.character(HorM))))) %>%
		mutate(HorM=(factor(gsub("CE", 
			"CENTER-EMBEDDING", as.character(HorM))))) %>%
		mutate(HorM=(factor(gsub("CR", 
			"CROSSING", as.character(HorM))))) %>%
		mutate(HorM=(factor(gsub("TR", 
			"TAIL-RECURSION", as.character(HorM))))) 


m.trace.3$CE_tot <- signif(m.trace.3$CE_tot,1)
m.trace.3$CR_tot <- signif(m.trace.3$CR_tot,1)

m.trace.3 <- m.trace.3 %>% group_by(Who) %>% 
			mutate(sumP=sum(P)) %>% 
			mutate(normP=P/sumP)  %>%
			mutate(dif_cecr=sum(CE_tot)-sum(CR_tot))


m.trace.3 <- m.trace.3 %>% 
			transform(Who=factor(reorder(Who, -dif_cecr)))



#m.trace.3 <- m.trace.3 %>% mutate(normP=P/sumP)



j_wit <-0.03
p.trace.3 <- ggplot(m.trace.3, aes(x=factor(CE_tot-CR_tot),
						 group=Who, y=normP)) +
							  #+
			#geom_jitter(stat='identity', 
			aes(group=Who, fill=Who) +

			stat_summary(fun.y=sum,
				position='dodge',
				geom="bar")		

p.trace.3 <- p.trace.3 +
				 t4	+ 
			scale_x_discrete(labels=c("Non-Recursive",
								 "Recursive")) +
			ylab("Aggregate Posterior Predictive Probability")
ggsave("output/trace3.pdf", width=10, height=10)



####


m.trace.3 <- m.trace.3 %>% 
			transform(Who=factor(reorder(Who, -CE_tot)))


p.trace.4 <- ggplot(m.trace.3, aes(x=factor(CE_tot),
						 group=Who, y=normP)) +
							  #+
			#geom_jitter(stat='identity', 
			aes(group=Who, fill=Who) +

			stat_summary(fun.y=sum,
				position='dodge',
				geom="bar")	

p.trace.4 <- p.trace.4 +
				 t4	+ 
			scale_x_discrete(labels=c("No C-E",
								 "Partial C-E",
								 "Full C-E")) +
			ylab("Aggregate Posterior Predictive Probability Over Strategy Types")

ggsave("output/trace4.pdf", width=10, height=10)
