# Don't worry about the warnings from this library.
library(dplyr)

library(ggplot2)
library(reshape2)

get_scouting_data <- function(input_filename) {
    #
    # This function reads the scouting data, cleans some of the data and renames the columns.
    #
    data <- read.csv(input_filename)

    # Set empty cells to 0
    data[is.na(data)] <- 0

    # Convert Climbing column to a numeric value.
    NumericClimb <- list('Level 3' = 12, 'Level 2' = 6, 'Level 1' = 3, 'Not on HAB' = 0)
    data[,'Climbing'] <- mapply(function(x) NumericClimb[[x]], as.vector(data[,'Climbing']))

    # Convert percentage column to numeric value
    data[,'FractionDefense'] <- mapply(function(x) as.numeric(strsplit(x, '%')), as.vector(data[,'X..of.time.playing.defense']))

    data <- rename(data,
        ScoutName = Your.name..FirstName.LastName.,
        Match = Match..,
        Team = Team..,
        StartPosition = Starting.Position,
        StartLevel2 = Starts.on.Level.2.,
        MovedSandstorm = Moved.during.sandstorm.,
        HabLine = Crossed.HAB.line.,
        StartingPiece = Starts.with,
        HatchSandstorm = X..of.hatch.panels.placed.during.sandstorm,
        CargoSandstorm = X..of.cargo.placed.during.sandstorm,
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
        PercentDefense = X..of.time.playing.defense,
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
    #
    # This summary function is the same as used in the Google spreadsheet.
    #
    scouting_data <- get_scouting_data(input_filename)

    intermediate_summary <- summarise(group_by(scouting_data, Match, Team),
        HabScore = mean(3 * HabLine * (StartLevel2 + 1)),
        HatchScore = 2 * mean(HatchSandstorm + HatchCargoBay + HatchBottomRocket + HatchMidRocket + HatchTopRocket),
        CargoScore = 3 * mean(CargoSandstorm + CargoCargoBay + CargoBottomRocket + CargoMidRocket + CargoTopRocket),
        ClimbScore = mean(Climbing)
    )

    summary_data <- summarise(group_by(intermediate_summary, Team),
        HabScore = mean(HabScore),
        HatchScore = mean(HatchScore),
        CargoScore = mean(CargoScore),
        ClimbScore = mean(ClimbScore),
        Score = HabScore + HatchScore + CargoScore + ClimbScore
    )

    # Sort the teams by total score in descending order.
    sorted_data <- arrange(summary_data, desc(Score))

    return(sorted_data)
}

get_summary_data_poc <- function(input_filename) {
    #
    # This summary function is just a proof of concept.
    #
    scouting_data <- get_scouting_data(input_filename)

    intermediate_summary <- summarise(group_by(scouting_data, Match, Team),
        HabScore = mean(3 * HabLine * (StartLevel2 + 1)),
        HatchScore = 2 * mean(HatchSandstorm + HatchCargoBay + HatchBottomRocket + 1.1 * HatchMidRocket + 1.1 * HatchTopRocket),
        CargoScore = 3 * mean(CargoSandstorm + CargoCargoBay + CargoBottomRocket + 1.1 * CargoMidRocket + 1.1 * CargoTopRocket),
        ClimbScore = mean(Climbing),
        DefenseScore = mean(10 * DefenseSuccess * FractionDefense / 100),
        FoulScore = mean(3 * Fouls + 10 * TechFouls),
        FailScore = 5 * mean(RobotDisabled + RobotFail + TippedOver + Reckless),
    )

    # Find the worst Foul/Fail score
    max_foul_fail <- max(summarise(group_by(intermediate_summary, Team), Total = mean(FoulScore + FailScore))$Total)

    summary_data <- summarise(group_by(intermediate_summary, Team),
        HabScore = mean(HabScore),
        HatchScore = mean(HatchScore),
        CargoScore = mean(CargoScore),
        ClimbScore = mean(ClimbScore),
        DefenseScore = mean(DefenseScore),
        OppPenaltyScore = max_foul_fail - mean(FoulScore + FailScore),
        Score = HabScore + HatchScore + CargoScore + ClimbScore + DefenseScore + OppPenaltyScore
    )

    # Sort the teams by total score in descending order.
    sorted_data <- arrange(summary_data, desc(Score))

    return(sorted_data)
}

plot_scores <- function(summary_data, title = 'Scouting Data', horizontal = FALSE, output_filename = '') {
    #
    # Plot the scores (Google spreadsheet version)
    #
    summary_data <- melt(select(summary_data, Team:ClimbScore), id.vars = 'Team')
    summary_data$variable <- factor(summary_data$variable, levels = c('ClimbScore', 'CargoScore', 'HatchScore', 'HabScore'))

    if (horizontal) {
        p <- ggplot(data = summary_data, aes(x = reorder(Team, value), y = value, fill = variable)) + coord_flip()
    }
    else {
        p <- ggplot(data = summary_data, aes(x = reorder(Team, -value), y = value, fill = variable)) +
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    }

    p <- p + geom_bar(stat = 'identity', color = 'black') +
        labs(title = title, x = 'Team', y = 'Score') +
        theme(plot.title = element_text(hjust = 0.5)) +
        scale_fill_manual(name = '', values = c('green', 'yellow', 'red', 'blue'))

    print(p)

    # Add ggsave arguments width, height, and units to adjust output file size.
    if (output_filename != '') {
        ggsave(output_filename)
    }
}

plot_scores_poc <- function(summary_data, title = 'Scouting Data (POC)', horizontal = FALSE, output_filename = '') {
    #
    # Plot the scores (proof of concept)
    #
    summary_data <- melt(select(summary_data, Team:OppPenaltyScore), id.vars = 'Team')
    summary_data$variable <- factor(summary_data$variable, levels = c('OppPenaltyScore', 'DefenseScore', 'ClimbScore', 'CargoScore', 'HatchScore', 'HabScore'))

    if (horizontal) {
        p <- ggplot(data = summary_data, aes(x = reorder(Team, value), y = value, fill = variable)) + coord_flip()
    }
    else {
        p <- ggplot(data = summary_data, aes(x = reorder(Team, -value), y = value, fill = variable)) +
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
    }

    p <- p + geom_bar(stat = 'identity', color = 'black') +
        labs(title = title, x = 'Team', y = 'Score') +
        theme(plot.title = element_text(hjust = 0.5)) +
        scale_fill_manual(name = '', values = c('tan3', 'magenta', 'green', 'yellow', 'red', 'blue'))

    print(p)

    # Add ggsave arguments width, height, and units to adjust output file size.
    if (output_filename != '') {
        ggsave(output_filename)
    }
}

plot_total_score <- function(summary_data, title = 'Scouting Data', horizontal = FALSE, output_filename = '') {
    #
    # Plot the total score
    #
    summary_data <- select(summary_data, Team, Score)

    if (horizontal) {
        p <- ggplot(data = summary_data, aes(x = reorder(Team, Score), y = Score, fill = Score)) + coord_flip()
    }
    else {
        p <- ggplot(data = summary_data, aes(x = reorder(Team, -Score), y = Score, fill = Score)) +
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
    }

    p <- p + geom_bar(stat = 'identity', color = 'black') +
        labs(title = title, x = 'Team', y = 'Score') +
        theme(plot.title = element_text(hjust = 0.5))

    print(p)

    # Add ggsave arguments width, height, and units to adjust output file size.
    if (output_filename != '') {
        ggsave(output_filename)
    }
}
