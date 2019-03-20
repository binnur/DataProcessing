library(dplyr)

get_scouting_data <- function(input_filename) {
    data <- read.csv(input_filename)

    # Set empty cells to 0
    data[is.na(data)] <- 0

    # Convert Climbing column to a numeric value.
    NumericClimb <- list('Level 3' = 12, 'Level 2' = 6, 'Level 1' = 3, 'Not on HAB' = 0)
    data[,'Climbing'] <- mapply(function(x) NumericClimb[[x]], as.vector(data[,'Climbing']))

    data <- rename(data,
        ScoutName = Your.name..FirstName.LastName.,
        Match = Match..,
        Team = Team..,
        StartPosition = Starting.Position,
        StartLevel2 = Starts.on.Level.2.,
        MovedSandstorm = Moved.during.sandstorm.,
        HabLine = Crossed.HAB.line.,
        StartHatch = Starts.with,
        HatchSandstorm = X..of.hatch.panels.placed.during.sandstorm,
        HatchSandstorm = X..of.cargo.placed.during.sandstorm,
        CargoSandstorm = X..of.cargo.placed.during.sandstorm,
        HatchPanels = Hatch.Panels,
        HatchCargoBay = X..of.hatch.panels.on.the.cargo.bay,
        HatchBottomRocket = X..of.hatch.panels.on.the.bottom.of.the.rocket,
        HatchMidRocket = X..of.hatch.panels.on.the.middle.of.the.rocket,
        HatchTopRocket = X..of.hatch.panels.on.the.top.of.the.rocket,
        HatchMissed = X..of.hatch.panels.missed,
        CargoCargoBay = X..of.cargo.in.the.cargo.bay,
        CargoBottomRocket = X..of.cargo.in.the.bottom.of.the.rocket,
        CargoMidRocket = X..of.cargo.in.the.middle.of.the.rocket,
        CargoTopRocket = X..of.cargo.in.the.top.of.the.rocket,
        CargoMissed = X..of.cargo.missed,
        DefenseSuccess = Played.defense.successfully,
        WeakDefense = Weak.to.defense..tippy..easily.pushed..etc.,
        BadStuff = Bad.stuff,
        Fouls = X..of.fouls,
        TechFouls = X..of.tech.fouls,
        RobotDisabled = Robot.disabled,
        RobotFail = Robot.failure,
        TippedOver = Tipped.over,
        Reckless = Reckless.driving,
        YellowCard = Yellow.card,
        RedCard = Red.card
    )

    return(data)
}

get_summary_data <- function(input_filename) {
    scouting_data <- get_scouting_data(input_filename)

    summary_data <- summarise(group_by(scouting_data, Team),
        StartScore = mean(0.5 * StartLevel2 + 0.5 * MovedSandstorm + 0.25 * HabLine + 1.3 * HatchSandstorm + 1.3 * CargoSandstorm),
        HatchScore = mean(HatchCargoBay + HatchBottomRocket + 1.1 * HatchMidRocket + 1.2 * HatchTopRocket - HatchMissed),
        CargoScore = mean(CargoCargoBay + CargoBottomRocket + 1.1 * CargoMidRocket + 1.2 * CargoTopRocket),
        ClimbScore = mean(Climbing),
        DefenseScore = mean(2.5 * DefenseSuccess),
        FoulScore = mean(Fouls + 2 * TechFouls + 2 * YellowCard + 4 * RedCard),
        FailScore = mean(5 * RobotDisabled + 3 * RobotFail + 3 * TippedOver + Reckless),
        Score = StartScore + HatchScore + CargoScore + ClimbScore + DefenseScore
    )

    sorted_data <- arrange(summary_data, desc(Score))

    return(sorted_data)
}

plot_scores <- function(match_data, output_filename='') {
    if (output_filename != '') {
        png(output_filename, width=640, height=500)
    }

    plot_data <- t(cbind(match_data[,'StartScore'], match_data[,'HatchScore'], match_data[,'CargoScore'], match_data[,'ClimbScore'], match_data[,'DefenseScore']))

    barplot(plot_data, names = match_data$Team,
        xlab = 'Team', ylab='Score', main='Team Scores',
        legend = c('Sandstorm Score', 'Hatch Score', 'Cargo Score', 'Climb Score', 'Defense Score'),
        col = c('blue', 'red', 'yellow', 'magenta', 'green'), las=2)

    if (output_filename != '') {
        dev.off()
    }
}

plot_fail_foul <- function(match_data, output_filename='') {
    if (output_filename != '') {
        png(output_filename, width=640, height=500)
    }

    plot_data <- t(cbind(match_data[,'FoulScore'], match_data[,'FailScore']))

    barplot(plot_data, names = match_data$Team,
        xlab = 'Team', ylab='Fail/Foul Score', main='Team Fail/Foul Scores',
        legend = c('Foul Score', 'Fail Score'),
        col = c('darkgreen', 'darkred'), las=2)

    if (output_filename != '') {
        dev.off()
    }
}

plot_total_score <- function(match_data, output_filename='') {
    if (output_filename != '') {
        png(output_filename, width=640, height=500)
    }

    barplot(t(match_data[,'Score']), names = match_data$Team,
        xlab = 'Team', ylab='Score', main='Team Score',
        legend = 'Score',
        col = 'cyan', las=2)

    if (output_filename != '') {
        dev.off()
    }
}
