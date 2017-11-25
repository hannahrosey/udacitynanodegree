#Data source: DrivenData Boxplots for Education Project, https://www.drivendata.org/competitions/46/box-plots-for-education-reboot/page/86/#labels_list
#Project Goal: create a multi-class classiciation model to quickly line items in school budgets by category
#Current Goal: explore and visualize dataset

#Read In
setwd('/Users/hgorman/Desktop/udacity_nanodegree/eda-course-materials/lesson6')
edat <- read.csv('open-education-data.csv')
str(edat)
  # Total = cost
  # ERS adds labels manually for the following (the model should predict them):
      #Function
      #Object_Type
      #Operating Status
      #Position Type
      #Pre-K
      #Reporting
      #Sharing
      #Student_Type
      #Use
  # Let's visualize the different FUNCTIONS first on their own

levels(edat$Function)
function_counts <- edat %>% group_by(Function) %>% summarise(count=n())
function_plot <- ggplot(data=function_counts,aes(x=function_counts$Function,y=function_counts$count)) +
  geom_bar(stat='identity') +
  theme_bw() + 
  coord_flip() +
  xlab("Function") +
  ylab("Frequency") +
  theme(axis.text.y=element_text(size=6)) +
  ggtitle("Number of Line Items by Function")
  
	# The most common costs by far are Teacher and Substitute compensation
	# Let's look at the frequency of other labels for compensation costs

compensation <- subset(edat,edat$Function=="Teacher Compensation"|edat$Function=="Substitute Compensation")
summary(compensation$Object_Type)

	# All substitute compensation will have the object_type "substitute compensation," so let's only look at object_type 	for teacher compensation
	
compensation <- compensation[compensation$Object_Type!='Substitute Compensation',]

compensation_counts <- compensation %>% group_by(Object_Type) %>% summarise('Count'=n())

compensation_plot <- ggplot(data=compensation_counts,aes(x=Object_Type,y=Count)) +
  geom_bar(stat='identity') +
  theme_bw() +
  coord_flip() +
  xlab("Function") +
  ylab("Frequency")

	# Some values for cost < 0; exclude

compensation_costs <- subset(compensation,compensation$Total>0)

cost_plot <- ggplot(data=subset(compensation_costs,compensation_costs$Object_Type!="NO_LABEL"),aes(x=Total,fill=Object_Type)) + 
  geom_histogram() +
  scale_x_log10() +
  xlab("Log of Total Cost") +
  ylab("Frequency") +
  ggtitle("Total Compensation Cost by Type") +
  theme_bw()
  
	# Save an image of the frequency of  
function_and_cost <- grid.arrange(function_plot,cost_plot)
ggsave(file="function and cost plots",'jpeg',plot=function_and_cost)

compensation_costs_base <- compensation_costs[compensation_costs$Object_Type=="Base Salary/Compensation"&compensation_costs$Function=="Teacher Compensation",]
base_pay_plot <- ggplot(data=compensation_costs_base,
                        aes(x=Total)) +
  geom_histogram(binwidth=5000) +
  coord_trans(limx=seq(0,150000)) +
  xlab("Teacher Compensation ($)") +
  ylab("Number of Line Items")

#Why are there so many line items for teachers pay under $10,000?

comp_base_slim <- compensation_costs_base[,c(6,23)] %>% group_by(Student_Type) %>% summarise(total_cost = sum(Total),mean_cost = mean(Total),median_cost = median(Total)) %>% gather(key='metric','value',c(mean_cost,median_cost))
head(comp_base_slim)

comp_bar <- ggplot(data=subset(comp_base_slim,comp_base_slim$Student_Type!="NO_LABEL"),aes(x=Student_Type,y=value,fill=metric),position_fill()) + 
  geom_bar(stat='identity') +
  coord_flip() +
  ylab('Mean/Median Cost per Instructor Compensation Line Item') +
  xlab('Student Type')

comp_box <- ggplot(data=subset(compensation_costs_base,compensation_costs_base$Total<=150000&compensation_costs_base$Student_Type!="NO_LABEL"),aes(x=Student_Type,y=Total)) +
  geom_boxplot() +
  ylab("Cost per Line Item") +
  xlab("Student Type") +
  theme_bw()

total_bar <- ggplot(data=subset(comp_base_slim,comp_base_slim$Student_Type!="NO_LABEL"),aes(x=Student_Type,y=total_cost)) +
  geom_bar(stat='identity') +
  coord_flip() +
  ylab("Total Compensation Cost, All Line Items ($)") +
  xlab("Student Type")

comp_plots <- grid.arrange(comp_box,total_bar,nrow=2)
ggsave(file='compensation spending by student type','jpeg',plot=comp_plots)

