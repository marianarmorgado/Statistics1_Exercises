load(url("http://ph.emu.ee/~ktanel/DK_0016/students.RData"))

# ---1---

students$bmi <- students$weight / sqrt((students$height)*100)

students_male <- students[students$gender=="2",]
students_female <- students[students$gender == "1",]

windows()
par(mfrow = c(2, 1)) # create a grid of 2 x 1 in the plot window
    hist(students_female$bmi,
        xlab = "female"
        )
     hist(students_male$bmi,
        xlab = "male"
        )

# ---2---
attach(students)

mean(bmi [gender == 2], na.rm = TRUE)
mean(bmi [gender == 1], na.rm = TRUE)
sd(bmi[gender == 2], na.rm = TRUE)
sd(bmi[gender == 1], na.rm = TRUE)
t.test(bmi)


# ---3---
students$sport01 <- factor(sport, levels = 1:5, labels = c(0,1,1,1,1))

students_na_rm <- students[-c(65), ]

chisq.test(students$sport01, p = 0.75)
