library(ggplot2)
df.ts = data.frame(ModelName = c('TS Ensemble', 'Theta', 'snaive', 'stl', 'ETS', 'auto.arima', 'rwf'),
    RMSE = c(16.51246, 17.28723, 19.63182, 20.85651, 37.75413, 45.81771, 51.86268))

ggplot(data = df.ts, aes(x = reorder(ModelName, RMSE), y = RMSE, fill = ModelName)) +
  geom_bar(stat='identity') +
  labs(title="Timeseries model performance") +
  xlab('Models') +
  ylab('RMSE') +
  geom_text(aes(y=RMSE-5, label=round(RMSE,2)), color='white', size=3) +
  coord_flip()

df.linear = data.frame(ModelName = c('elastic net', 'ridge', 'lm', 'pcr', 'pls'),
                   RMSE = c(5.781435, 5.781667, 5.869585, 5.869585, 5.875254)) 

ggplot(data = df.linear, aes(x = reorder(ModelName, RMSE), y = RMSE, fill = ModelName)) +
  geom_bar(stat='identity') +
  labs(title="ML Linear model performance") +
  xlab('Models') +
  ylab('RMSE') +
  geom_text(aes(y=RMSE-0.5, label=round(RMSE,2)), color='white', size=3) +
  coord_flip()

df.nonlinear = data.frame(ModelName = c('mars', 'svm_poly', 'svm_linear', 'knn', 'svm_radial', 'neural net'),
                       RMSE = c(4.434103, 4.464866, 6.003718, 7.902959, 8.939123, 55.240198)) 

ggplot(data = df.nonlinear, aes(x = reorder(ModelName, RMSE), y = RMSE, fill = ModelName)) +
  geom_bar(stat='identity') +
  labs(title="ML Nonlinear model performance") +
  xlab('Models') +
  ylab('RMSE') +
  geom_text(aes(y=RMSE-2, label=round(RMSE,2)), color='white', size=3) +
  coord_flip()

df.tree = data.frame(ModelName = c('boosted trees', 'random forest'),
                          RMSE = c(4.439231, 5.391929))
ggplot(data = df.tree, aes(x = reorder(ModelName, RMSE), y = RMSE, fill = ModelName)) +
  geom_bar(stat='identity') +
  labs(title="ML Tree-based model performance") +
  xlab('Models') +
  ylab('RMSE') +
  geom_text(aes(y=RMSE-2, label=round(RMSE,2)), color='white', size=3) +
  coord_flip()

df.over = rbind(df.ts, df.linear, df.nonlinear, df.tree)
ggplot(data = df.over, aes(x = reorder(ModelName, RMSE), y = RMSE, fill = ModelName)) +
  geom_bar(stat='identity') +
  labs(title="Overall model performance") +
  xlab('Models') +
  ylab('RMSE') +
  geom_text(aes(y=RMSE-2, label=round(RMSE,2)), color='white', size=3) +
  coord_flip()
