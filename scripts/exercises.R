load(url("http://ph.emu.ee/~ktanel/DK_0016/students.RData"))

#-----1-----
# Calculate the students’ body mass index (as a new variable)...

students$bmi <- students$weight / ((students$height / 100)^2)

# ... and study its distribution separately for men and women
# Does the body mass index follow the normal distribution?

attach(students)
summary(bmi)
windows(8, 10)
par(mfrow = c(2, 1))
    hist(bmi[gender == "1"],
        main = "BMI of women",
        xlab = "BMI",
        xlim = c(15, 40),
        ylim = c(0, 200),
        col = "#DDAAC4"
        )
     hist(bmi[gender == "2"],
        main = "BMI of men",
        xlab = "BMI",
        xlim = c(15, 40),
        ylim = c(0, 50),
        col = "#AAC7DD"
        )