## Recommendation System on movie rating (R, Excel)
---
#### Description
### Final results are summarized in Excel file.

●Collaborative filtering to predicts user ratings and generate movie recommendations for focal users.

●k-Nearest Neighbors algorithm (Pearson correlation coefficient as the similarity measure), and latent factor algorithm was used in prediction.

●Mean Absolute Percent Error (MAPE) was used to assess performance of algorithms. 

---
#### Movie Rating Data

The Movie ratings file contains ratings of 8 movies given by 22 moviegoers. The ratings range from 1 being “Hated it” to 5 being “Loved it”. 
Two of the moviegoers in this file - Thomas and Rosie – are your “focal” users for whom you will be making predictions.

---
#### Results and Comments:
KNN Assessment: In this case, model K=3 has the lowest MAPE. Thus, Rosie may rate 3.04 and 2.32 for Iron Man and The Heat point respectively. 
Thomas may rate 2.5 and 4.36 for Gravity and Hunger Games respectively.

Latent Factor Model Assessment: The performance of 5 latent factors is better than that of 3 latent factors. In this case, Rosie may have a rate of 2.67 for movie Iron Man, 2.66 for movie The Heat. Thomas may have a rate of 3.7 for Gravity and 4.85 for Hunger Games. The latent factor algorithm generate better performance than knn-based method and naïve(average) method.

---
#### Author Info

- Linkedin- [@Yiyang Wang](https://www.linkedin.com/in/yiyangwang0715/)
- Facebook- [@Yiyang Wang](https://www.facebook.com/profile.php?id=100010881825806)
