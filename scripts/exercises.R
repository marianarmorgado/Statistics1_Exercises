load(url("http://ph.emu.ee/~ktanel/DK_0016/students.RData"))

#-----1-----
# Calculate the students’ body mass index (as a new variable)...

students$bmi <- students$weight / sqrt(students$height / 100)

# ... and study its distribution separately for men and women
# Does the body mass index follow the normal distribution?

attach(students)
windows()
par(mfrow = c(2, 1))
    hist(bmi[gender == "1"],
        xlab = "BMI",
        main = "BMI of women"
        )
     hist(bmi[gender == "2"],
        xlab = "BMI",
        main = "BMI of men"
        )