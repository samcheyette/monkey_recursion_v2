library(ggplot2)
library(reshape)
library(grid)
library(dplyr)

t1 <- 	theme(axis.text=element_text(size=13), 
		strip.text.x = element_text(size = 10),
		plot.title=element_text(size=18),
		axis.text.x=element_blank(),
		#axis.text.x=element_text(size=13, angle=45),
		axis.title.x=element_blank(),
		axis.text.y=element_text(size=12),

		legend.title=element_blank(),
		legend.text=element_text(size=10),
		 legend.key.size = unit(4, 'lines'))

data = read.csv("output/all_data8.csv")

m = melt(data, id=c("Who", "Participant","HorM",
					 "which", "var", "P"))

m$Participant <- factor(m$Participant)



####################TRACE1###########################
m <- cbind(m,
				rand=runif(nrow(m), -0.00001,0.00001))


for (i in 1:3) {
subs <- levels(m$Who)[i]

m.trace.1 <- m %>% filter(grepl(subs, 
			as.character(Who)))

p_what <- function(x, y, what) {
	if (grepl(what,as.character(x))) {
		return(y)

	} else {
		return(0.0)
	}
}



m.trace.1 <- m.trace.1 %>%
			mutate(P = P + rand) 



m.trace.1 <- m.trace.1 %>%
			mutate(var=gsub("W", "(", var)) %>%
			mutate(var=gsub("X", "[", var)) %>%
			mutate(var=gsub("Y", "]", var)) %>%
			mutate(var=gsub("Z", ")", var)) %>%
			mutate(var=gsub(" ", "", var))


m.trace.1 <- m.trace.1 %>%
			filter(!grepl("distr", which)) %>% 
			group_by(Participant, HorM, var) %>%
			mutate(p_mod = p_what(HorM, P, "model")) %>% 
			mutate(ce_mod=p_what(HorM, P, "CE")) %>%
			group_by(Participant, var) %>% 
			mutate(p_sum=sum(p_mod)) %>%
			mutate(ce_sum = sum(ce_mod)) %>%
			group_by(Participant, var, HorM) %>%
			mutate(p_type=p_sum*P) %>%
			ungroup %>%
			filter(!grepl("model", HorM))






m.trace.1 <- m.trace.1 %>% 
			group_by(Participant, HorM) %>%
			top_n(6, wt=p_sum) %>%
			ungroup %>%
			mutate(var2=paste(var, 
				as.character(as.numeric(p_sum)))) %>%
			#group_by(Participant) %>%
			transform(var2 = factor(reorder(var2, -p_sum))) %>%

			transform(Participant = factor(reorder(Participant, 
										-ce_sum))) %>%
			ungroup



print(head(m.trace.1))

m.trace.1 <- m.trace.1 %>% 
		mutate(HorM=(factor(gsub("OT", 
			"OTHER", as.character(HorM))))) %>%
		mutate(HorM=(factor(gsub("CE", 
			"CENTER-EMBEDDING", as.character(HorM))))) %>%
		mutate(HorM=(factor(gsub("CR", 
			"CROSSING", as.character(HorM))))) %>%
		mutate(HorM=(factor(gsub("TR", 
			"TAIL-RECURSION", as.character(HorM))))) 


ymax <- max(m.trace.1$p_sum)
p.trace.1 <- ggplot(data=m.trace.1, aes(x=var2, y=p_type,
			 group=HorM)) +
			geom_bar(stat='identity',aes(fill=HorM)) +
			geom_text(aes(label=var,x=var2,y=0),
				 size=5.0, angle=45) +
			facet_wrap(~Participant, 
				scale="free_x") +
			ylab("Probability") +
			ggtitle(paste(subs, 
				"Strategies and Their Properties")) +

			t1  +
			ylim(-0.1*ymax,ymax+0.01)

out <- paste("output/trace1", subs)	
out <- gsub(" ", "", paste(out, ".pdf"))
ggsave(out, width=16, height=9)

}
######################################################

#############################################
###################DIST########################

m.dist <- m %>% filter(grepl("distr", which)) %>% 
			mutate(var=gsub( "W", "(", var)) %>%
			mutate(var=gsub( "X", "[", var)) %>%
			mutate(var=gsub( "Y", "]", var)) %>%
			mutate(var=gsub( "Z", ")", var)) 



thresh <- 0.1






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
			facet_wrap(~Participant,
						scales="free_x")

p.dist.1 <- p.dist.1 + 
				theme(axis.text.x =
				 element_text(angle = 90, hjust = -1)) +
				t1


ggsave("output/dist1.pdf", width=10, height=7)

##################################################
#######################TRACE######################