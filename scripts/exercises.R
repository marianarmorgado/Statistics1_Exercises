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
        students$beer[students$gender == 2]),
    simulate = TRUE
)

windows(7, 6)
plot(
    table(
        students$sport[students$gender == 2],
        students$beer[students$gender == 2]),
    las = 1,
    main = "Frequency of sports vs Frequency of beer in male students",
    xlab = "Sports frequency",
    ylab = "Number of beers",
    col = c("#FCF5B3", "#FFF489", "#FFF060", "#FFEB29", "#FFE700")
)