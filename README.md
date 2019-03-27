# Project: Can you unscramble a blurry image? 
![image](figs/example.png)

### [Full Project Description](doc/project3_desc.md)

Term: Spring 2019

+ Team 7

	+ Liu, Siwei sl4224@columbia.edu

	+ Shen, Yu ys3167@columbia.edu

	+ Shu, Jason jls2319@columbia.edu

	+ Wang, Xinzhu xw2581@columbia.edu

	+ Zhong, Ming mz2692@columbia.edu


+ Project summary: In this project, we created a classification engine to enhance the resolution of images. The baseline model is a GBM (Gradient Boosting Machine) model, and we used XGboost in order to enhance the images faster. In summary, for the baseline model, the time for training the model is around 53 minutes, and the time for super-resolution is around 13 minutes. For the XGboost model, the time for training the model is around 15 minutes and the time for super-resolution is around 4 minutes.

The source code of the baseline model is in [this folder](doc/main_baseline.Rmd) and the source code of the XGboost model is in [this folder](doc/main_xgb.Rmd)
	
**Contribution statement**: ([default](doc/a_note_on_contributions.md)) 

+Zhong, Ming(major): Ming is responsible to run the baseline model and modify any codes related to baseline mode. Ming is also responsible to organize codes and make final submission of the whole project. 

+Wang, Xinzhu(major): Xinzhu is responsible to run the XGboost model and modify any codes related to XGboost model. Xiznhu also assisted Shen to debug the super resolution part. 

+Shen, Yu(major): Yu is responsible to finish the superResolution part and modify the codes related to this part. Yu also help with running the XGboost model and get the final result of this part.

+Liu, Siwei(Average Participation): Siwei partook the feature improvement part. She formed the discussion group and participated throughout the project discussion. Siwei also wrapped up the project summary in the readme file.

Following [suggestions](http://nicercode.github.io/blog/2013-04-05-projects/) by [RICH FITZJOHN](http://nicercode.github.io/about/#Team) (@richfitz). This folder is orgarnized as follows.

```
proj/
├── lib/
    └──superResolution.R
    └──psnr.R
    └──test.R
    └──test_xgb.R
    └──train.R
    └──train_xgb.R
    └──feature.R
    └──cross_validation.R
├── data/
    └──SR-B
    └──SR-I
├── doc/
    └── main_baseline.Rmd
    └── main_xgb.Rmd
├── figs/
└── output/
```

Please see each subfolder for a README file.
