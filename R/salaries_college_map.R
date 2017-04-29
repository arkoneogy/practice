library(XML)

dataURL= "http://online.wsj.com/public/resources/documents/info-Salaries_for_Colleges_by_Region-sort.html"
datapull= readHTMLTable(dataURL, stringsAsFactors = F)

# str(mydata)
salaries= datapull[[7]]
# rm(datapull)

cc= names(salaries)
cc= gsub(" ", ".", cc)
names(salaries)= cc

library(data.table)

salaries = data.table(salaries)
str(salaries)

fixStrings = function(x)
{
  retval = substr(x, 1, nchar(x)-2)
  return(retval)
}

fixSalaries = function(x)
{
  r1= gsub("\\$", "", x)
  r2= gsub(",", "", r1)
  r3= as.numeric(r2)
  return(r3)
}

salaries = salaries[, lapply(.SD, fixStrings), .SDcols = cc]

cc= cc[3:8]
schools = salaries[, c(1,2), with=F]
amounts = salaries[, cc, with=F]

amounts = amounts[, lapply(.SD, fixSalaries), .SDcols = cc]
salaries = cbind(schools, amounts)

str(salaries)

schools.loc = geocode(schools$School.Name)
schools.loc$School.Name = schools$School.Name

salaries = merge(salaries, schools.loc, by= "School.Name", all.x = T)
str(salaries)


library(ggmap)

states = map_data("state")

l1= grep("alaska", salaries$School.Name, ignore.case = T)
l2= grep("hawaii", salaries$School.Name, ignore.case = T)

salaries = salaries[ -c(l1,l2)]

library(RColorBrewer)

yor_col = brewer.pal(6, "YlOrRd")


theFig = ggplot(salaries) +
  
  geom_polygon(aes(x = long, y = lat, group = group),
               data = states, fill = "navy",
               color = "white") +
  
  geom_point(aes(x = lon, y = lat,
                 color = Starting.Median.Salary, text = School.Name)) +
  
  coord_fixed(1.3) +
  
  scale_color_gradientn(name = "Starting Median Salary",
                        colors = rev(yor_col)) +
  # labels = comma) +
  
  guides(size = FALSE) +
  
  theme_bw() +
  
  theme(axis.text = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank())

theFig

library(plotly)

ggplotly(theFig, tooltip = c("text", "Starting.Median.Salary"),
         width = 1280, height = 720)
