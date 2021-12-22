load(url("http://ph.emu.ee/~ktanel/DK_0016/students.RData"))

#-----1-----
# Calculate the students’ body mass index (as a new variable)...

students$bmi <- students$weight / ((students$height / 100)^2)

# ... and study its distribution separately for men and women
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

#-----2-----
# What is the average (± standard deviation) body mass index of men and women?

mean(students$bmi [students$gender == 1], na.rm = TRUE)
sd(students$bmi[students$gender == 1], na.rm = TRUE)
mean(students$bmi [students$gender == 2], na.rm = TRUE)
sd(students$bmi[students$gender == 2], na.rm = TRUE)

# Is the difference statistically significant?...
# ... Make the decision both based on the 95% confidence interval...
# ... of means’ difference and based on the p-value.

t.test(bmi ~ gender, data = students)

#-----3-----
#Create a new variable ’sport01’ with value zero,
# if student does not practice sport (sport=1), and value one,
# if student practices sport (sport>1).
#You can use the following command:

students$sport01 <- factor(
    students$sport,
    levels = 1:5,
    labels = c(0, 1, 1, 1, 1)
)

# How big is the percentage of students practicing sport
#(with 95% confidence interval)? Is the percentage of
#students practicing sport statistically significant from 75%?

table(students$sport01)
prop.test(
    c(124, 535),
    c(124 + 535, 124 + 535)
)
prop.test(535, 660, p = 0.75)