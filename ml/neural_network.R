neuralnet


train_data %>%  head()

nn_model <- neuralnet(output ~ close_dividends + 
   lucro_liquido + 
   roe + 
   roi + 
   popl + 
   poat + 
   roa + 
   gaf + 
   ebit + 
   ebitda,
 data = train_data %>% na.omit,
 hidden = c(10, 6),
 linear.output = F,
 lifesign = 'full',
 rep=20
)




plot(nn_model, col.hidden = 'darkgreen',     
     col.hidden.synapse = 'darkgreen',
     show.weights = F,
     information = F,
     fill = 'lightblue')
