load(url("http://ph.emu.ee/~ktanel/DK_0016/students.RData"))

#--------------------------------------------------------------
#-----1-----
# Calculate the students’ body mass index (as a new variable)

students$bmi <- students$weight / ((students$height / 100)^2)

# and study its distribution separately for men and women
# Does the body mass index follow the normal distribution?

summary(students$bmi)
windows(8, 4)
par(mfrow = c(1, 2))
    hist(students$bmi[students$gender == "1"],
        main = "BMI of women",
        xlab = "BMI",
        xlim = c(15, 40),
        ylim = c(0, 200),
        col = "#DDAAC4"
    )
     hist(students$bmi[students$gender == "2"],
        main = "BMI of men",
        xlab = "BMI",
        xlim = c(15, 40),
        ylim = c(0, 50),
        col = "#AAC7DD"
    )

#--------------------------------------------------------------
#-----2-----
# What is the average (± standard deviation) body mass index of men and women?

mean(students$bmi [students$gender == 1], na.rm = TRUE)
sd(students$bmi[students$gender == 1], na.rm = TRUE)
mean(students$bmi [students$gender == 2], na.rm = TRUE)
sd(students$bmi[students$gender == 2], na.rm = TRUE)

# Is the difference statistically significant?
# Make the decision both based on the 95% confidence interval
# of means’ difference and based on the p-value.

t.test(bmi ~ gender, data = students)

#--------------------------------------------------------------
#-----3-----
# Create a new variable ’sport01’ with value zero,
# if student does not practice sport (sport=1), and value one,
# if student practices sport (sport>1).
# You can use the following command:

students$sport01 <- factor(
    students$sport,
    levels = 1:5,
    labels = c(0, 1, 1, 1, 1)
)

# How big is the percentage of students practicing sport
# (with 95% confidence interval)? Is the percentage of
# students practicing sport statistically significant from 75%?

table(students$sport01)
prop.test(
    c(124, 535),
    c(124 + 535, 124 + 535)
)
prop.test(535, 660, p = 0.75)

#--------------------------------------------------------------
#-----4-----
# What about the percentage of sporting students among men and women?
# Are these proportions statistically significantly different?

table(
    students$gender,
    students$sport01 == "1"
)
prop.test(
    c(408, 127),
    c(408 + 103, 127 + 21)
)

#--------------------------------------------------------------
#-----5-----
# Are the body mass index and sporting associated?
# Test the statistical significance of body mass index difference
# between sporting and non-sporting students.

t.test(
    students$bmi[students$sport01 == 1],
    students$bmi[students$sport01 == 0]
)

# Visualize the result

windows(6, 6)
boxplot(
    bmi ~ sport01,
    data = students,
    names = c("non-sporting", "sporting"),
    xlab = "students",
    ylab = "BMI",
    col = "#5d7c4d"
)

#--------------------------------------------------------------
#-----6-----
# Are the sporting and health associated?
# Test, visualize and comment the result.

prop.table(
    table(
        students$sport,
        students$health
    ),
    1
)
chisq.test(
    table(
        students$sport,
        students$health
    ),
    simulate = TRUE
)

windows(6, 6)
plot(
    table(students$sport, students$health),
    las = 1,
    main = "Health of students based on sports frequency",
    xlab = "Sports frequency",
    ylab = "Health",
    col = c("#5d7c4d", "#dddb62", "#e2a759", "#ce3c3c")
)

#--------------------------------------------------------------
#-----7-----
# Are the beer consumption and sporting of male students associated?

prop.table(
    table(
        students$sport[students$gender == 2],
        students$beer[students$gender == 2]
    ),
    1
 )
chisq.test(
    table(
        students$sport[students$gender == 2],
        students$beer[students$gender == 2]
    ),
    simulate = TRUE
)

windows(7, 6)
plot(
    table(
        students$sport[students$gender == 2],
        students$beer[students$gender == 2]
    ),
    las = 1,
    main = "Frequency of sports vs Frequency of beer in male students",
    xlab = "Sports frequency",
    ylab = "Number of beers",
    col = c("#FCF5B3", "#FFF489", "#FFF060", "#FFEB29", "#FFE700")
)

#--------------------------------------------------------------
#-----8-----
# Create a new variable ’smoke012’ with value zero,
# if the student does not smoke (smoking=1),
# one, if the student no longer smokes but has smoked (smoking=2),
# and two, if the student
# smokes (smoking>2). Does the smoking depend on gender?
# Visualize the result.

students$smoking <- factor(students$smoking)
levels(students$smoking)
students$smoke012 <- factor(
    students$smoking,
    levels = c(1, 2, 3, 4, 5, 6, 7),
    labels = c("0", "1", "2", "2", "2", "2", "2")
)
prop.table(
    table(students$gender, students$smoke012),
    1
)
chisq.test(
    table(students$gender, students$smoke012),
    simulate = TRUE
)
students$gendernames <- factor(
    students$gender,
    levels = 1:2,
    labels = c("women", "men")
)
windows(6, 6)
plot(
    table(students$smoke012, students$gendernames),
    main = "Smoking in female and male students",
    xlab = "smoking status",
    ylab = "gender",
    col = c("#DDAAC4", "#AAC7DD")
)

#--------------------------------------------------------------
#-----9-----
# Are student body mass index and systolic and diastolic blood pressure related?
# Whether these relationships are different between men and women
# (no statistical significance testing of
# difference is required, but you can visualize the results)?
library("corrplot")

# all students
cor(
    x = students [, c("bmi", "SVR", "DVR")],
    use = "complete.obs"
)
correlations <- cor(
    x = students [, c("bmi", "SVR", "DVR")],
    use = "complete.obs"
)
windows(6, 6)
corrplot(
    correlations,
    method = "square",
)

# female students
students_f <- students[students$gender == "1", ]
cor(
    x = students_f [, c("bmi", "SVR", "DVR")],
    use = "complete.obs"
)
correlations_f <- cor(
    x = students_f [, c("bmi", "SVR", "DVR")],
    use = "complete.obs"
)
windows(6, 6)
corrplot(
    correlations_f,
    method = "square",
)

# male students
students_m <- students[students$gender == "2", ]
cor(
    x = students_m [, c("bmi", "SVR", "DVR")],
    use = "complete.obs"
)
correlations_m <- cor(
    x = students_m [, c("bmi", "SVR", "DVR")],
    use = "complete.obs"
)
windows(6, 6)
corrplot(
    correlations_m,
    method = "square",
)

#--------------------------------------------------------------
#-----10-----
# Predict students’ systolic blood pressure based on gender and body mass index.
# Illustrate the result of the modeling.
# What is the expected systolic blood pressure of male and female students
# with a body mass index of 20 points? But with body mass index of 25 points?
# Is it necessary to  consider also the gender by body mass index interaction?
# But sporting and/or smoking?

model1 <- lm(SVR ~ gender + bmi, data = students)
summary(model1)

windows()
par(mfrow = c(2, 2))
    plot(model1, 1)
    plot(model1, 2)
    plot(model1, 4)
    plot(model1, 5)

pred_svr_bmi20_f <- predict(
    model1,
    data.frame(bmi = 20, gender = 1)
)

pred_svr_bmi20_m <- predict(
    model1,
    data.frame(bmi = 20, gender = 2)
)

pred_svr_bmi25_f <- predict(
    model1,
    data.frame(bmi = 25, gender = 1)
)

pred_svr_bmi25_m <- predict(
    model1,
    data.frame(bmi = 25, gender = 2)
)

windows()
plot(
    SVR ~ bmi,
    data = students,
    xlab = "BMI"
)
    points(
        x = 20,
        y = pred_svr_bmi20_f,
        col = "#DDAAC4",
        pch = 16,
        cex = 2
    )
    points(
        x = 20,
        y = pred_svr_bmi20_m,
        col = "#AAC7DD",
        pch = 16,
        cex = 2
    )
    points(
        x = 25,
        y = pred_svr_bmi25_f,
        col = "#DDAAC4",
        pch = 18,
        cex = 3
    )
    points(
        x = 25,
        y = pred_svr_bmi25_m,
        col = "#AAC7DD",
        pch = 18,
        cex = 3
    )

model2 <- lm(SVR ~ gender * bmi, data = students)
summary(model2)

anova(model1, model2)
sqrt(56185 / 437) # model 1
sqrt(56176 / 436) # model 2

model3 <- lm(SVR ~ gender + bmi + sport + smoking, data = students)
summary(model3)

step_model3 <- step(model3, direction = "both")