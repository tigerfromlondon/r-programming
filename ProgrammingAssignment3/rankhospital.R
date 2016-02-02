rankhospital <- function(state, outcome, num) {
        my_data <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", stringsAsFactors = FALSE)
        outcomes <- c("heart attack"=11, "heart failure"=17, "pneumonia"=23)
        if (outcome %in% names(outcomes) == FALSE) {
                print("invalid outcome")
                stop()
        }
        if (state %in% my_data$State == FALSE) {
                print("invalid state")
                stop()
        }
        sub <- my_data[, c(2, 7, outcomes[outcome])]
        names(sub) <- c("hos","stat","out")
        com <- na.omit(sub)
        sor <- com[order(com$stat,com$out,com$hos),]
        spl <- split(sor, sor$stat)
        id <- spl[stat = state]
        po <- as.data.frame(id)
        if(num == "best") {
                num <- 1
        }
        if(num == "worst") {
                num <- nrow(po)
        }
        to <- po[num, ]
        #print(head(id))
        print(to)
}