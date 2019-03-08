## for the dashboard ##

## For leaf 1
library(leaflet)
library(ggdag)
library(dagitty)
g <- dagitty('dag {
             Pathology [pos="0,3"]
             Slope [pos="1,2"]
             Survival [pos="1,4"]
             CR [pos="1,1"]
             BaseBrain [pos="2,4"]
             CogLevel [pos="2,3"]
             TrueCog [pos="2,2"]
             CogScore [pos="2,1"]
             Diagnosis [pos="2,0"]
             ADL [pos="4,1.25"]
             Error [pos="3,1"]
            
             Pathology->Survival
             Pathology->Slope->TrueCog->CogScore->Diagnosis  
             Pathology->Slope->TrueCog->ADL
             CR->CogScore->Diagnosis
             ADL->Diagnosis
             BaseBrain->CogLevel->TrueCog->CogScore->Diagnosis
             BaseBrain->CogLevel->Survival
             Error->CogScore->Diagnosis
             }')

#####################
##### Plots #########
#####################
library(dplyr)
dag0 = g
dag_tidy = tidy_dagitty(dag0)
variables = unique(dag_tidy$data$name)
ancestors = lapply(variables, function(x){ancestors(dag0,x)})
descendants=lapply(variables, function(x){descendants(dag0,x)})
parents=lapply(variables, function(x){parents(dag0,x)})
names(ancestors) = names(descendants) = names(parents) = variables
exog = variables[unlist(lapply(ancestors,length))==1]
endog = variables[!(variables%in%exog)]

df_var = dag_tidy$data %>% rowwise() %>%
  mutate(VariableType =ifelse(name %in%exog, 'Exogenous','Endogenous'))

### Themes ###
guideDAG = guides(color = guide_legend(override.aes = list(size = 2)))
themeDAG = theme(legend.position = "bottom", legend.text = element_text(size = 10))

### Plot Exogenous ###

g_exog = df_var%>%
  ggplot(aes(x = x, y = y, xend = xend, yend = yend, color = VariableType)) +
  geom_dag_node(size = 13.5) +
  geom_dag_edges() +
  geom_dag_text(col = "white",size = 2) +
  theme_dag() +
  scale_dag(breaks  = c('Exogenous','Endogenous')) +
  guideDAG +
  themeDAG
#g_exog
##########################


g_pathology = dag_tidy %>% 
  node_descendants(.var=c("Pathology")) %>%
  ggplot(aes(x = x, y = y, xend = xend, yend = yend, color = descendant)) +
  geom_dag_node(size = 13.5) +
  geom_dag_edges() +
  geom_dag_text(col = "white",size = 2) +
  theme_dag() +
  scale_dag(breaks  = c("descendant", "ancestor"))

g_CR = dag_tidy %>% 
  node_descendants(.var=c("CR")) %>%
  ggplot(aes(x = x, y = y, xend = xend, yend = yend, color = descendant)) +
  geom_dag_node(size = 13.5) +
  geom_dag_edges() +
  geom_dag_text(col = "white",size = 2) +
  theme_dag() +
  scale_dag(breaks  = c("descendant", "ancestor")) #  ignores NA in legend

g_baseBrain =  dag_tidy %>% 
  node_descendants(.var=c("BaseBrain")) %>%
  ggplot(aes(x = x, y = y, xend = xend, yend = yend, color = descendant)) +
  geom_dag_node(size = 13.5) +
  geom_dag_edges() +
  geom_dag_text(col = "white",size = 2) +
  theme_dag() +
  scale_dag(breaks  = c("descendant", "ancestor")) #  ignores NA in legend
g_error =  dag_tidy %>% 
  node_descendants(.var=c("Error")) %>%
  ggplot(aes(x = x, y = y, xend = xend, yend = yend, color = descendant)) +
  geom_dag_node(size = 13.5) +
  geom_dag_edges() +
  geom_dag_text(col = "white",size = 2) +
  theme_dag() +
  scale_dag(breaks  = c("descendant", "ancestor")) #  ignores NA in legend

g_parent = function(var1){
  dag_tidy %>% 
  node_parents(.var=c(var1)) %>%
  ggplot(aes(x = x, y = y, xend = xend, yend = yend, color = parent)) +
  geom_dag_node(size = 13.5) +
  geom_dag_edges() +
  geom_dag_text(col = "white",size = 2) +
  theme_dag() +
  scale_dag(breaks  = c("parent", "child")) +
  guideDAG +
  themeDAG
}

