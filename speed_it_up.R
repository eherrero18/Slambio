
library(tidyverse)
library(ggplot2)
library(RODBC)
library(DBI)
library(odbc)
library(RPostgreSQL)
library(RPostgreSQL)
library(ggforce)
library(data.table)
library(reactable)

# pw <- { "Ve@26989134"}
# myconn <- DBI::dbConnect(odbc::odbc(),"Snowflake", SNOWFLAKE_UID = "eherrero@texasrangers.com", pwd=pw)
# rm(pw)
# 
# SQLSERVER_SERVER = "txr-arl-scttst2"
# SQLSERVER_DB = "Scouting"
# sqldbcon <- odbcDriverConnect(paste('driver={SQL Server};server=',SQLSERVER_SERVER,';database=',SQLSERVER_DB,';trusted_connection=true', sep="") )
# 
# POSTGRES_IP = "35.194.54.205"
# POSTGRES_DB = "postgres"
# POSTGRES_PORT = 5432
# POSTGRES_UID = "postgres"
# POSTGRES_PASSWORD = "P@loDur0!"
# postgres_connection <- dbConnect(dbDriver("PostgreSQL"), dbname = POSTGRES_DB, host = POSTGRES_IP, port = POSTGRES_PORT, user = POSTGRES_UID, password = POSTGRES_PASSWORD)

############################### POPULATION ########################################################

teams <- data.frame(
  "Team" = c("Arizona Diamondbacks", "Atlanta Braves", "Baltimore Orioles",  "Boston Red Sox",  "Chicago Cubs", 
             "Cincinnati Reds", "Cleveland Guardians","Colorado Rockies","Chicago White Sox","Detroit Tigers",
             "Houston Astros","Kansas City Royals","Los Angeles Angels","Los Angeles Dodgers","Miami Marlins", 
             "Milwaukee Brewers", "Minnesota Twins", "New York Mets","New York Yankees", "Oakland Athletics",
             "Philadelphia Phillies","Pittsburgh Pirates","San Diego Padres","Seattle Mariners","San Francisco Giants",
             "St. Louis Cardinals","Tampa Bay Rays","Texas Rangers","Toronto Blue Jays","Washington Nationals"),
  
  "Abbrev" = c("ARI", "ATL","BAL","BOS","CHC","CIN", "CLE", "COL", "CWS", "DET", "HOU", "KCR", "LAA", "LAD",
               "MIA", "MIL", "MIN", "NYM", "NYY","OAK", "PHI", "PIT","SDP", "SEA", "SFG", "STL", "TBR","TEX","TOR","WAS"),
  "Color" = c("#A71930", "#CE1141","#DF4601", "#BD3039","#CC3433", "#C6011F",
              "#E50022", "#333366","#C4CED4", "#FA4616", "#EB6E1F", "#BD9B60", "#BA0021", "#005A9C",
              "#00A3E0",  "#FFC52F", "#D31145", "#FF5910", "#C4CED3", "#EFB21E", "#E81828",
              "#FDB827", "#2F241D", "#005C5C", "#FD5A1E" , "#C41E3A", "#F5D130","#C0111F",
              "#E8291C", "#AB0003" ),
  "Shortname" = c("dbacks", "braves","orioles", "redsox", "cubs", "reds", "guardians", "rockies", "whitesox",
                  "tigers", "astros", "royals", "angels", "dodgers", "marlins", "brewers", "twins", "mets",
                  "yankees", "athletics", "phillies", "pirates", "padres", "mariners", "giants",
                  "cardinals", "rays", "rangers", "bluejays", "nationals"),
  "number" = c(109, 144, 110, 111, 112, 
               113, 114, 115, 145 , 116,
               117,118, 108 , 119, 146 ,
               158, 142 , 121, 147 , 133,
               143, 134, 135, 136, 137,
               138, 139, 140, 141,120)
  
)

BuYlRd <- function(x){
  lol <-  ifelse(x >= .90, "#d10202",
                 ifelse(between(x, .80,.90), "#ff0000", 
                        ifelse(between(x, .70,.80), "#ff6666",
                               ifelse(between(x, .62,.70), "#ff9999",
                                      ifelse(between(x, .55, .62), "#ffb3b3",
                                             ifelse(between(x, .50, .55), "#ffcccc",
                                                    ifelse(between(x, .45,.50), "#bfbfff",
                                                        ifelse(between(x, .38, .45), "#a3a3ff",
                                                           ifelse(between(x, .30,.38), "#7879ff",
                                                                  ifelse(between(x, .20,.30), "#4949ff",
                                                                         ifelse(between(x,.10,.20), "#1f1fff","#0000ff"
                                                                                )
                                                                         )
                                                                  )
                                                           )
                                                        )
                                                    )
                                             )
                                      )
                               )
                        )
                 )
  return(lol)
}
 

pitch_type_data <- read.csv("pitch_type_data.csv")

pitch_subtype_data <- read.csv("pitch_subtype_data.csv")

lva_data <- read.csv("lva_data.csv")

data <- read.csv("main_options_data.csv")

data$TEAM[data$TEAM == "TB"] <- "TBR"
data$TEAM[data$TEAM == "SF"] <- "SFG"
data$TEAM[data$TEAM == "SD"] <- "SDP"
data$TEAM[data$TEAM == "LA"] <- "LAD"
data$TEAM[data$TEAM == "ANA"] <- "LAA"
data$TEAM[data$TEAM == "KC"] <- "KCR"

# #####################CREATE CHOICES TO DIFFERENTIATE BETWEEN SAME NAME PLAYERS ##########################
choices_pop <- function()
{
  data %>% filter(YEAR == 2022) %>% group_by(ID) %>% filter(LASTDAY == max(LASTDAY)) -> use
  temp <-  paste(use$NAME, " (", use$TEAM, ")", sep = "" )
  return(temp)
}
  
########################################################################################################

################ PROSPECT RANKINGS ################################# PROSPECTS RSNKINGS ####################

ranking = read.csv("ranking_data.csv")

player_rank <- function(ID)
{

  ranking %>% filter(PlayerID == ID) -> EV
  avg_rank <- mean(EV$PlayerRank)

  pipe_rank <- EV$PlayerRank[EV$Source == "MLBPipeline"]
  fg_rank <- EV$PlayerRank[EV$Source == "Fangraphs"]
  bp_rank <- EV$PlayerRank[EV$Source == "BaseballProspectus"]
  ba_rank <- EV$PlayerRank[EV$Source == "BaseballAmerica"]


  ranks <- list("mean" = avg_rank, "pipe" = pipe_rank, "fg" = fg_rank, "ba" = ba_rank, "bp" = bp_rank)
  return(ranks)
}

#############################################################################################################



################################## GET THE PLAYERS BIO #######################################################

bio_text = read.csv("bio_data.csv")

#############################################################################################################


############## PLAYERS PROJECTED 22 AND 23 WAR 25th, 50th, anf 75th PERCENTILE #################################


projs <- read.csv("projections_data.csv")

war <- read.csv("war_data.csv")

############################### PERFORMACE DATA ########################################################################

# CURPIT YEAR NEW ONE
# this one has no teams together

alone_years <- read.csv("alone_years_data.csv")

together_years <- read.csv("together_years_data.csv")

performance = rbind(alone_years, together_years)

performance$ORGS[performance$ORGS == "TB"] <- "TBR"
performance$ORGS[performance$ORGS == "SF"] <- "SFG"
performance$ORGS[performance$ORGS == "SD"] <- "SDP"
performance$ORGS[performance$ORGS == "LA"] <- "LAD"
performance$ORGS[performance$ORGS == "ANA"] <- "LAA"
performance$ORGS[performance$ORGS == "KC"] <- "KCR"

performance %>%
    mutate(ORGS = ifelse(str_length(performance$ORGS) > 3, "All", ORGS)) -> performance

########################################################################################################

pitch_type_table <- function(ID_, lvl_, pitch_types_use, pitch_subtypes_use, use, use_subtypes, hexcol, team)
{
  gas <- use %>%
    reactable(
      defaultColDef = colDef(align = 'center',
                             headerStyle = list(background = hexcol, color = ifelse(
                               team %in% c("SDP", "SEA", "COL"),
                               "#FFFFFF", "#000000"
                             )
                            )
      ),
      details = function(index)
      {
        use_subtypes %>%
          filter(PITCH_TYPE == use[index,]$PITCH_TYPE) %>%
          filter(YEAR == use[index,]$YEAR) %>%
          select(YEAR, PITCH_TYPE, PITCH_SUBTYPE, TOT, VELO, HM, IVB, SPINRATE,SWM, CHASERATE,
                 STR_RATE, PG, VAA, BIP, NITRORATE, GBRATE) -> tbl
        tbl %>%
          select(PITCH_TYPE, PITCH_SUBTYPE, TOT, VELO, HM, IVB, SPINRATE, SWM, CHASERATE,
                 STR_RATE, PG, VAA, BIP, NITRORATE, GBRATE) %>%
          reactable(
            columns = list(
              PITCH_TYPE = colDef(name = "Type", minWidth = 90),
              PITCH_SUBTYPE = colDef(name = "Subtype", minWidth = 120),
              TOT = colDef(name = "#", minWidth = 55),
              SWM = colDef(name = "SWM%", minWidth = 80),
              VELO = colDef(name = "Velo.", minWidth = 60),
              SPINRATE = colDef(name = "Spin", minWidth = 60),
              CHASERATE =colDef(name = "CH%", minWidth = 70),
              STR_RATE = colDef(name = "Str%", minWidth = 60),
              PG = colDef(name = "Grade", minWidth = 70),
              NITRORATE = colDef(name = "Nitro%", minWidth = 75),
              GBRATE = colDef(name = "GB%", minWidth = 60),
              BIP = colDef(name = "BIP", minWidth = 50),
              VAA = colDef(name = "VAA", minWidth = 60),
              HM = colDef(minWidth = 70),
              IVB = colDef(minWidth = 60)
            ),
            width = 1050,
            highlight = TRUE,
            bordered = TRUE,
            striped = TRUE
          )

      },
      columns = list(
        YEAR = colDef(name = "Year", minWidth = 70),
        PITCH_TYPE = colDef(name = "Type", minWidth = 90),
        PITCH_SUBTYPE = colDef(name = "Subtype", minWidth = 130),
        TOT = colDef(name = "#", minWidth = 60),
        HM = colDef(name = "HM", minWidth = 50),
        IVB = colDef(name = "IVB", minWidth = 50),
        SPINRATE = colDef(name = "Spin", minWidth = 60,
                          style = function(value, index)
                          {

                            pitch_types_use %>%
                              filter(ID == ID_) %>%
                              filter(PITCH_TYPE == use[index,]$PITCH_TYPE) %>%
                              filter(YEAR == use[index,]$YEAR) %>%
                              filter(LVL == lvl_) %>%
                              pull(spin_ntile) -> ntile
                            color = BuYlRd(ntile)
                            list(background = color, color = ifelse(!between(ntile, .20, .80), "#FFFFFF", "#000000"))

                          }),
        SWM = colDef(name = "SWM%", minWidth = 70,
                     style = function(value, index)
                     {

                       pitch_types_use %>%
                         filter(ID == ID_) %>%
                         filter(PITCH_TYPE == use[index,]$PITCH_TYPE) %>%
                         filter(YEAR == use[index,]$YEAR) %>%
                         filter(LVL == lvl_) %>%
                         pull(swm_ntile) -> ntile
                       color = BuYlRd(ntile)
                       list(background = color, color = ifelse(!between(ntile, .20, .80), "#FFFFFF", "#000000"))

                     }),
        CHASERATE = colDef(name = "CH%", minWidth = 70,
                           style = function(value, index)
                           {

                             pitch_types_use %>%
                               filter(ID == ID_) %>%
                               filter(PITCH_TYPE == use[index,]$PITCH_TYPE) %>%
                               filter(YEAR == use[index,]$YEAR) %>%
                               filter(LVL == lvl_) %>%
                               pull(ch_ntile) -> ntile
                             color = BuYlRd(ntile)
                             list(background = color, color = ifelse(!between(ntile, .20, .80), "#FFFFFF", "#000000"))

                           }),
        VAA = colDef(name = "VAA", minWidth = 70),
        PG = colDef(name = "Grade", minWidth = 70,
                    style = function(value, index)
                    {

                      pitch_types_use %>%
                        filter(ID == ID_) %>%
                        filter(PITCH_TYPE == use[index,]$PITCH_TYPE) %>%
                        filter(YEAR == use[index,]$YEAR) %>%
                        filter(LVL == lvl_) %>%
                        pull(pg_ntile) -> ntile
                      color = BuYlRd(ntile)
                      list(background = color, color = ifelse(!between(ntile, .20, .80), "#FFFFFF", "#000000"))

                    }),
        NITRORATE = colDef(name = "Nitro%", minWidth = 80),
        GBRATE = colDef(name = "GB%", minWidth = 70,
                        style = function(value, index)
                        {
                          pitch_types_use %>%
                            filter(ID == ID_) %>%
                            filter(PITCH_TYPE == use[index,]$PITCH_TYPE) %>%
                            filter(YEAR == use[index,]$YEAR) %>%
                            filter(LVL == lvl_) %>%
                            pull(gb_ntile) -> ntile
                          color = BuYlRd(ntile)
                          list(background = color, color = ifelse(!between(ntile, .20, .80), "#FFFFFF", "#000000"))
                        }),
        STR_RATE = colDef(name = "Str%", minWidth = 60),
        BIP = colDef(minWidth = 70),
        VELO = colDef(name = "Velo.", minWidth = 70,
                      style = function(value, index)
                      {
                        pitch_types_use %>%
                          filter(ID == ID_) %>%
                          filter(PITCH_TYPE == use[index,]$PITCH_TYPE) %>%
                          filter(YEAR == use[index,]$YEAR) %>%
                          filter(LVL == lvl_) %>%
                          pull(velo_ntile) -> ntile
                        color = BuYlRd(ntile)
                        list(background = color, color = ifelse(!between(ntile, .20, .80), "#FFFFFF", "#000000"))
                      })
      ),
      width = 1210,
      fullWidth = TRUE,
      pagination = FALSE
    )
  return(gas)
}


location <- read.csv("location_data.csv")

#coord <- read.csv("coordinates_data.csv")


########################################################################################################################
########################################################################################################################
##################################             LEADERBOARDS             ################################################
########################################################################################################################
########################################################################################################################


main_ldr <- read.csv("main_ldr_data.csv")

main_ldr$TEAM[main_ldr$TEAM == "SF"] <- "SFG"
main_ldr$TEAM[main_ldr$TEAM == "SD"] <- "SDP"
main_ldr$TEAM[main_ldr$TEAM == "LA"] <- "LAD"
main_ldr$TEAM[main_ldr$TEAM == "ANA"] <- "LAA"
main_ldr$TEAM[main_ldr$TEAM == "KC"] <- "KCR"
main_ldr$TEAM[main_ldr$TEAM == "TB"] <- "TBR"

mnfa <- read.csv("mnfa_40_data.csv")


sinker_fb_tab <- function(team, pitch, use)
{
  if(pitch == "Fastball")
  {
    col_name = "VAA:\n FB Up"
  }else{
    col_name = "VAA:\n SI Dw"
  }
  
  use$PG <- round(use$PG, 1)
  use$LVA <- round(use$LVA, 1)
  use$VELO <- round(use$VELO,1)
  use$HM <- round(use$HM,1)
  use$IVB <- round(use$IVB,1)
  use$SPINRATE <- round(use$SPINRATE)
  use$SWM <- paste0(round(use$SWM, 1), "%", sep = "")
  use$CHASERATE <- paste0(round(use$CHASERATE, 1), "%", sep = "")
  use$VAA <- round(use$VAA, 2)
  
  use$hm_n <- round(use$hm_n*100)
  use$pg_n <- round(use$pg_n*100)
  use$ch_n <- round(use$ch_n*100)
  use$velo_n <- round(use$velo_n*100)
  use$LVA_n <- round(use$LVA_n*100)
  use$IVB_n <- round(use$IVB_n*100)
  use$spin_n <- round(use$spin_n*100)
  use$swm_n <- round(use$swm_n*100)
  use$vaa_n <- round(use$vaa_n*100)
  
  df_col <- data.frame(
    g_th = use$pg_n,
    vel_th = use$velo_n,
    spi_n = use$spin_n,
    hm_th = use$hm_n,
    ivb_th = use$IVB_n,
    lva_th = use$LVA_n,
    swm_th = use$swm_n,
    ch_th = use$ch_n,
    vaa_th = use$vaa_n
  ) %>% mutate(col_swm = BuYlRd(swm_th/100),
               col_ch =  BuYlRd(ch_th/100),
               col_g =   BuYlRd(g_th/100),
               col_vel =  BuYlRd(vel_th/100),
               col_spi = BuYlRd(spi_n/100),
               col_hm = BuYlRd(hm_th/100),
               col_ivb = BuYlRd(ivb_th/100),
               col_lva = BuYlRd(lva_th/100),
               col_vaa = BuYlRd(vaa_th/100)
  ) -> df_col
  
  df_col_g <- df_col[order(df_col[,1]),]
  df_col_vel <- df_col[order(df_col[,2]),]
  df_col_spi <- df_col[order(df_col[,3]),]
  df_col_hm <- df_col[order(df_col[,4]),]
  df_col_ivb <- df_col[order(df_col[,5]),]
  df_col_lva <- df_col[order(df_col[,6]),]
  df_col_swm <- df_col[order(df_col[,7]),]
  df_col_ch <- df_col[order(df_col[,8]),]
  df_col_vaa <- df_col[order(df_col[,9]),]

  
p <- use %>%
    select(TEAM, NAME, PITCHER_THROWS, PG, pg_n, VELO, velo_n, SPINRATE, spin_n,
           HM, hm_n, IVB, IVB_n, LVA, LVA_n, SWM, swm_n, CHASERATE, ch_n, VAA, vaa_n) %>%
    reactable(
      columns =  list(
        TEAM = colDef(name = "", align = 'center', minWidth = 70,
                      cell = function(value) {
                        num <- teams$number[teams$Abbrev == value]
                        link <- paste("https://www.mlbstatic.com/team-logos/team-cap-on-light/", num, ".svg", sep ="")
                        image <- img(src = link, style = "height: 24px;", alt = "")
                        tagList(
                          div(style = "display: inline-block; width: 45px;", image))
                      }),
        NAME = colDef(name = "Name", align = 'center', minWidth = 150),
        PITCHER_THROWS = colDef(name = "Throws", align = 'center', minWidth = 80),
        PG = colDef(name = "Grade", align = 'center', minWidth = 80),
        pg_n = colDef(name = "n", align = 'center', minWidth = 100,
                      cell = data_bars(., 
                                       text_size = 13, 
                                       box_shadow = TRUE,
                                       text_position = 'center',
                                       force_outside = c(0,27),
                                       min_value = 0,
                                       max_value = 100,
                                       fill_color = df_col_g$col_g
                      )
                    ),
        VELO =  colDef(name = "Velo", align = 'center', minWidth = 80),
        velo_n = colDef(name = "n", align = 'center', minWidth = 100,
                        cell = data_bars(., 
                                         text_size = 13, 
                                         box_shadow = TRUE,
                                         text_position = 'center',
                                         force_outside = c(0,27),
                                         min_value = 0,
                                         max_value = 100,
                                         fill_color = df_col_vel$col_vel
                        )),
        SPINRATE = colDef(name= "Spin", align = 'center', minWidth = 80),
        spin_n = colDef(name = "n", align = 'center', minWidth = 100,
                        cell = data_bars(., 
                                         text_size = 13, 
                                         box_shadow = TRUE,
                                         text_position = 'center',
                                         force_outside = c(0,27),
                                         min_value = 0,
                                         max_value = 100,
                                         fill_color = df_col_spi$col_spi
                        )),
        HM = colDef(name = "HM", align = 'center', minWidth = 80),
        hm_n = colDef(name = "n", align = 'center', minWidth = 100,
               cell = data_bars(., 
                                text_size = 13, 
                                box_shadow = TRUE,
                                text_position = 'center',
                                force_outside = c(0,27),
                                min_value = 0,
                                max_value = 100,
                                fill_color = df_col_hm$col_hm
               )),
        IVB = colDef(name = "IVB", align = 'center', minWidth = 80),
        IVB_n =  colDef(name = "n", align = 'center', minWidth = 100,
                     cell = data_bars(., 
                                      text_size = 13, 
                                      box_shadow = TRUE,
                                      text_position = 'center',
                                      force_outside = c(0,27),
                                      min_value = 0,
                                      max_value = 100,
                                      fill_color = df_col_ivb$col_ivb
                     )),
        LVA = colDef( align = 'center', minWidth = 80),
        LVA_n = colDef(name = "n", align = 'center', minWidth = 100,
                       cell = data_bars(., 
                                        text_size = 13, 
                                        box_shadow = TRUE,
                                        text_position = 'center',
                                        force_outside = c(0,27),
                                        min_value = 0,
                                        max_value = 100,
                                        fill_color = df_col_lva$col_lva
                       )),
        SWM = colDef(name = "SWM%", align = 'center', minWidth = 90),
        swm_n = colDef(name = "n", align = 'center', minWidth = 100,
                       cell = data_bars(., 
                                        text_size = 13, 
                                        box_shadow = TRUE,
                                        text_position = 'center',
                                        force_outside = c(0,27),
                                        min_value = 0,
                                        max_value = 100,
                                        fill_color = df_col_swm$col_swm
                       )),
        CHASERATE = colDef(name = "CH%", align = 'center', minWidth = 80),
        ch_n = colDef(name = "n", align = 'center', minWidth = 100,
               cell = data_bars(., 
                                text_size = 13, 
                                box_shadow = TRUE,
                                text_position = 'center',
                                force_outside = c(0,27),
                                min_value = 0,
                                max_value = 100,
                                fill_color = df_col_ch$col_ch
               )),
        VAA = colDef(name = col_name, align = 'center', minWidth = 80),
        vaa_n = colDef(name = "n", align = 'center', minWidth = 100,
                       cell = data_bars(., 
                                        text_size = 13, 
                                        box_shadow = TRUE,
                                        text_position = 'center',
                                        force_outside = c(0,27),
                                        min_value = 0,
                                        max_value = 100,
                                        fill_color = df_col_vaa$col_vaa
                       ))
        
      ),
      width = 1940,
      filterable = TRUE,
      bordered = TRUE,
      defaultColDef = colDef(
        header = function(value) gsub(".", " ", value, fixed = TRUE),
        style = list(zIndex = 1),
        headerStyle = list(background = ifelse(team == "All","#D50032" ,teams$Color[teams$Abbrev == team]),
                           color = ifelse(teams$Abbrev[teams$Abbrev == team] %in% 
                                            c( "CWS", "KC", "MIL", "NYY", "TB", "PIT", "OAK"), 
                                          "#000000","#f6f8fa"))
      ),
      theme = reactableTheme(
        borderColor = ifelse(team == "All", "#002D72" ,teams$Color[teams$Abbrev == team] ),
        stripedColor = "#f6f8fa",
        cellPadding = "8px 12px",
        selectStyle = list(background = teams$Color[teams$Abbrev == team])
      )
    )
 return(p)
  
}


other_pitches <- function(team, pitch, use)
{
  
  
  use$PG <- round(use$PG, 1)
  use$LVA <- round(use$LVA, 1)
  use$VELO <- round(use$VELO,1)
  use$HM <- round(use$HM,1)
  use$IVB <- round(use$IVB,1)
  use$SPINRATE <- round(use$SPINRATE)
  use$SWM <- paste0(round(use$SWM, 1), "%", sep = "")
  use$CHASERATE <- paste0(round(use$CHASERATE, 1), "%", sep = "")

  use$hm_n <- round(use$hm_n*100)
  use$pg_n <- round(use$pg_n*100)
  use$ch_n <- round(use$ch_n*100)
  use$velo_n <- round(use$velo_n*100)
  use$LVA_n <- round(use$LVA_n*100)
  use$IVB_n <- round(use$IVB_n*100)
  use$spin_n <- round(use$spin_n*100)
  use$swm_n <- round(use$swm_n*100)

  df_col <- data.frame(
    g_th = use$pg_n,
    vel_th = use$velo_n,
    spi_n = use$spin_n,
    hm_th = use$hm_n,
    ivb_th = use$IVB_n,
    lva_th = use$LVA_n,
    swm_th = use$swm_n,
    ch_th = use$ch_n
  ) %>% mutate(col_swm = BuYlRd(swm_th/100),
               col_ch =  BuYlRd(ch_th/100),
               col_g =   BuYlRd(g_th/100),
               col_vel =  BuYlRd(vel_th/100),
               col_spi = BuYlRd(spi_n/100),
               col_hm = BuYlRd(hm_th/100),
               col_ivb = BuYlRd(ivb_th/100),
               col_lva = BuYlRd(lva_th/100)
  ) -> df_col
  
  df_col_g <- df_col[order(df_col[,1]),]
  df_col_vel <- df_col[order(df_col[,2]),]
  df_col_spi <- df_col[order(df_col[,3]),]
  df_col_hm <- df_col[order(df_col[,4]),]
  df_col_ivb <- df_col[order(df_col[,5]),]
  df_col_lva <- df_col[order(df_col[,6]),]
  df_col_swm <- df_col[order(df_col[,7]),]
  df_col_ch <- df_col[order(df_col[,8]),]
  
  use %>%
    select(TEAM, NAME, PITCHER_THROWS, PG, pg_n, VELO, velo_n, SPINRATE, spin_n,
           HM, hm_n, IVB, IVB_n, LVA, LVA_n, SWM, swm_n, CHASERATE, ch_n) %>%
    reactable(
      columns =  list(
        TEAM = colDef(name = "", align = 'center', minWidth = 70,
                      cell = function(value) {
                        num <- teams$number[teams$Abbrev == value]
                        link <- paste("https://www.mlbstatic.com/team-logos/team-cap-on-light/", num, ".svg", sep ="")
                        image <- img(src = link, style = "height: 24px;", alt = "")
                        tagList(
                          div(style = "display: inline-block; width: 45px;", image))
                      }),
        NAME = colDef(name = "Name", align = 'center', minWidth = 150),
        PITCHER_THROWS = colDef(name = "Throws", align = 'center', minWidth = 79),
        PG = colDef(name = "Grade", align = 'center', minWidth = 80),
        pg_n = colDef(name = "n", align = 'center', minWidth = 100,
                      cell = data_bars(., 
                                       text_size = 13, 
                                       box_shadow = TRUE,
                                       text_position = 'center',
                                       force_outside = c(0,27),
                                       min_value = 0,
                                       max_value = 100,
                                       fill_color = df_col_g$col_g
                      )
        ),
        VELO =  colDef(name = "Velo", align = 'center', minWidth = 70),
        velo_n = colDef(name = "n", align = 'center', minWidth = 100,
                        cell = data_bars(., 
                                         text_size = 13, 
                                         box_shadow = TRUE,
                                         text_position = 'center',
                                         force_outside = c(0,27),
                                         min_value = 0,
                                         max_value = 100,
                                         fill_color = df_col_vel$col_vel
                        )),
        SPINRATE = colDef(name= "Spin", align = 'center', minWidth = 80),
        spin_n = colDef(name = "n", align = 'center', minWidth = 100,
                        cell = data_bars(., 
                                         text_size = 13, 
                                         box_shadow = TRUE,
                                         text_position = 'center',
                                         force_outside = c(0,27),
                                         min_value = 0,
                                         max_value = 100,
                                         fill_color = df_col_spi$col_spi
                        )),
        HM = colDef(name = "HM", align = 'center', minWidth = 80),
        hm_n = colDef(name = "n", align = 'center', minWidth = 100,
                      cell = data_bars(., 
                                       text_size = 13, 
                                       box_shadow = TRUE,
                                       text_position = 'center',
                                       force_outside = c(0,27),
                                       min_value = 0,
                                       max_value = 100,
                                       fill_color = df_col_hm$col_hm
                      )),
        IVB = colDef(name = "IVB", align = 'center', minWidth = 80),
        IVB_n =  colDef(name = "n", align = 'center', minWidth = 100,
                        cell = data_bars(., 
                                         text_size = 13, 
                                         box_shadow = TRUE,
                                         text_position = 'center',
                                         force_outside = c(0,27),
                                         min_value = 0,
                                         max_value = 100,
                                         fill_color = df_col_ivb$col_ivb
                        )),
        LVA = colDef( align = 'center', minWidth = 80),
        LVA_n = colDef(name = "n", align = 'center', minWidth = 100,
                       cell = data_bars(., 
                                        text_size = 13, 
                                        box_shadow = TRUE,
                                        text_position = 'center',
                                        force_outside = c(0,27),
                                        min_value = 0,
                                        max_value = 100,
                                        fill_color = df_col_lva$col_lva
                       )),
        SWM = colDef(name = "SWM%", align = 'center', minWidth = 80),
        swm_n = colDef(name = "n", align = 'center', minWidth = 100,
                       cell = data_bars(., 
                                        text_size = 13, 
                                        box_shadow = TRUE,
                                        text_position = 'center',
                                        force_outside = c(0,27),
                                        min_value = 0,
                                        max_value = 100,
                                        fill_color = df_col_swm$col_swm
                       )),
        CHASERATE = colDef(name = "CH%", align = 'center', minWidth = 80),
        ch_n = colDef(name = "n", align = 'center', minWidth = 100,
                      cell = data_bars(., 
                                       text_size = 13, 
                                       box_shadow = TRUE,
                                       text_position = 'center',
                                       force_outside = c(0,27),
                                       min_value = 0,
                                       max_value = 100,
                                       fill_color = df_col_ch$col_ch
                      ))
        
      ),
      width = 1765,
      filterable = TRUE,
      bordered = TRUE,
      defaultColDef = colDef(
        header = function(value) gsub(".", " ", value, fixed = TRUE),
        style = list(zIndex = 1),
        headerStyle = list(background = ifelse(team == "All","#D50032" ,teams$Color[teams$Abbrev == team]),
                           color = ifelse(teams$Abbrev[teams$Abbrev == team] %in% 
                                            c( "CWS", "KC", "MIL", "NYY", "TB", "PIT", "OAK"), 
                                          "#000000","#f6f8fa"))
      ),
      theme = reactableTheme(
        borderColor = ifelse(team == "All", "#002D72" ,teams$Color[teams$Abbrev == team] ),
        stripedColor = "#f6f8fa",
        cellPadding = "8px 12px",
        selectStyle = list(background = teams$Color[teams$Abbrev == team])
      )
      
    )
}

################################## PLAYERS OF INTEREST PANEL ##################################################


get_facedf <- function(face_df, IDs, team)
{
  
  
  for(i in 1:length(IDs))
  {
    
  data %>% 
    filter(ID == IDs[i]) %>%
    select(AWAY_COMPETITION_LEVEL_ABBREV) %>%
    mutate("rank" = ifelse(AWAY_COMPETITION_LEVEL_ABBREV == "MLB", 1, 
                           ifelse(AWAY_COMPETITION_LEVEL_ABBREV == "AAA", 2, 
                                  ifelse(AWAY_COMPETITION_LEVEL_ABBREV == "AA", 3, 
                                         ifelse(AWAY_COMPETITION_LEVEL_ABBREV == "A+", 4, 
                                                ifelse(AWAY_COMPETITION_LEVEL_ABBREV == "A", 5, 
                                                       ifelse(AWAY_COMPETITION_LEVEL_ABBREV == "Rk", 6,
                                                              ifelse(AWAY_COMPETITION_LEVEL_ABBREV == "Rk-", 7, NA)))))))) %>%
    unique() %>% 
      arrange(rank) %>% 
        slice(1) %>%
        pull(AWAY_COMPETITION_LEVEL_ABBREV) -> main

    mlbid <- data$MLBID[data$ID == IDs[i]][1]
    
    if(main == "MLB")
    {
      link <- paste("https://midfield.mlbstatic.com/v1/people/", mlbid, "/spots/240?zoom=1.2", sep = "")
  
    }else if(main == "Rk-" || main == "Rk")
    {
      link <- paste("https://i.ebayimg.com/images/g/B9sAAOSwf25iOdgH/s-l500.png")
      
    }else{
      link <- paste("https://img.mlbstatic.com/mlb-photos/image/upload/c_fill,g_auto/w_180/v1/people/", mlbid, "/headshot/milb/current", sep = "")
    }
    
    face_df$Link[i] = link
  }
  
  return(face_df)
}

