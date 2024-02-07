# Packages ----------------------------------------------------------------
library(tidyverse)
library(openai)

media <- "src"
chat_prompt <- create_chat_completion(
  model = "gpt-4",
  messages = list(
    list(
      "role" = "system",
      "content" = "You are a helpful assistant trained to assist in categorizing open answers about media sources. Only answer with the medias names separated by a comma. Your available answer choices are: RDI, TVA Nouvelles, Le Devoir, La Presse, Radio-Canada (SRC), LCN, CTV, CBC, The Gazette, Journal de Québec, Journal de Montréal, Le Soleil, 98.5 FM, National Post, Globe and Mail, Global News, CNN, Fox News, Guardian. If the answer is not in these choices, ignore it."
    ),
    list(
      "role" = "user",
      "content" = paste0("Identify the specific media outlet for the following response: ", media, ".")
    )
  )
)

# February ----------------------------------------------------------------

feb <- readxl::read_excel("_SharedFolder_memoire-pot-growth/data/lake/omnibus/february/february_open.xlsx")

answers <- feb$O_C14

for (i in 1:length(answers)) {
  media <- answers[i]
  request_successful <- FALSE
  attempt_time <- 0
  
  print(paste("Traitement de la réponse :", media))
  
  while (!request_successful && attempt_time <= 5) {
    start_time <- Sys.time()
    
    chat_prompt <- tryCatch({
      R.utils::withTimeout({
        create_chat_completion(
          model = "gpt-4",
          messages = list(
            list(
              "role" = "system",
              "content" = "You are a helpful assistant trained to assist in categorizing open answers about media sources. Only answer with the medias names separated by a comma. Your available answer choices are: RDI, TVA Nouvelles, Le Devoir, La Presse, Radio-Canada (SRC), LCN, CTV, CBC, The Gazette, Journal de Québec, Journal de Montréal, Le Soleil, 98.5 FM, National Post, Globe and Mail, Global News, CNN, Fox News, Guardian. If the answer is not in these choices, ignore it: simply answer NA. Separate each media by a comma."
            ),
            list(
              "role" = "user",
              "content" = paste0("Identify the specific media outlet for the following response: ", media, ".")
            )
          )
        )
      }, timeout = 10)  # Timeout en secondes
    }, error = function(e) {
      cat("Erreur ou timeout pour la réponse :", i, "-", media, "\n")
      NULL  # En cas d'erreur ou de timeout, retourne NULL
    })
    
    end_time <- Sys.time()
    attempt_time <- as.numeric(end_time - start_time, units = "secs")
    
    if (!is.null(chat_prompt)) {
      request_successful <- TRUE
      cat("Réponse obtenue avec succès pour :", i, "-", media, "\n")
      message(paste0(media, " ==> ", chat_prompt[["choices"]][["message.content"]]))
      if (i == 1) {
        output <- chat_prompt[["choices"]][["message.content"]]
      } else {
        output <- c(output, chat_prompt[["choices"]][["message.content"]])
      }
    } else {
      cat("Tentative échouée, nouvelle tentative pour :", i, "-", media, "\n")
      Sys.sleep(2)  # Petite pause avant de réessayer
    }
  }
}

feb$O_C14_clean <- output

writexl::write_xlsx(feb, "_SharedFolder_memoire-pot-growth/data/lake/omnibus/february/february_open_clean.xlsx")

# March ----------------------------------------------------------------

march <- readxl::read_excel("_SharedFolder_memoire-pot-growth/data/lake/omnibus/march/march_open.xlsx")

answers <- march$O_C11

for (i in 1:length(answers)) {
  media <- answers[i]
  request_successful <- FALSE
  attempt_time <- 0
  
  print(paste("Traitement de la réponse :", media))
  
  while (!request_successful && attempt_time <= 5) {
    start_time <- Sys.time()
    
    chat_prompt <- tryCatch({
      R.utils::withTimeout({
        create_chat_completion(
          model = "gpt-4",
          messages = list(
            list(
              "role" = "system",
              "content" = "You are a helpful assistant trained to assist in categorizing open answers about media sources. Only answer with the medias names separated by a comma. Your available answer choices are: RDI, TVA Nouvelles, Le Devoir, La Presse, Radio-Canada (SRC), LCN, CTV, CBC, The Gazette, Journal de Québec, Journal de Montréal, Le Soleil, 98.5 FM, National Post, Globe and Mail, Global News, CNN, Fox News, Guardian. If the answer is not in these choices, ignore it: simply answer NA. Separate each media by a comma."
            ),
            list(
              "role" = "user",
              "content" = paste0("Identify the specific media outlet for the following response: ", media, ".")
            )
          )
        )
      }, timeout = 10)  # Timeout en secondes
    }, error = function(e) {
      cat("Erreur ou timeout pour la réponse :", i, "-", media, "\n")
      NULL  # En cas d'erreur ou de timeout, retourne NULL
    })
    
    end_time <- Sys.time()
    attempt_time <- as.numeric(end_time - start_time, units = "secs")
    
    if (!is.null(chat_prompt)) {
      request_successful <- TRUE
      cat("Réponse obtenue avec succès pour :", i, "-", media, "\n")
      message(paste0(media, " ==> ", chat_prompt[["choices"]][["message.content"]]))
      if (i == 1) {
        output <- chat_prompt[["choices"]][["message.content"]]
      } else {
        output <- c(output, chat_prompt[["choices"]][["message.content"]])
      }
    } else {
      cat("Tentative échouée, nouvelle tentative pour :", i, "-", media, "\n")
      Sys.sleep(2)  # Petite pause avant de réessayer
    }
  }
}

march$O_C11_clean <- output

writexl::write_xlsx(march, "_SharedFolder_memoire-pot-growth/data/lake/omnibus/march/march_open_clean.xlsx")

# April ----------------------------------------------------------------

april <- readxl::read_excel("_SharedFolder_memoire-pot-growth/data/lake/omnibus/april/april_open.xlsx")

answers <- april$O_C11

for (i in 1:length(answers)) {
  media <- answers[i]
  request_successful <- FALSE
  attempt_time <- 0
  
  print(paste("Traitement de la réponse :", media))
  
  while (!request_successful && attempt_time <= 5) {
    start_time <- Sys.time()
    
    chat_prompt <- tryCatch({
      R.utils::withTimeout({
        create_chat_completion(
          model = "gpt-4",
          messages = list(
            list(
              "role" = "system",
              "content" = "You are a helpful assistant trained to assist in categorizing open answers about media sources. Only answer with the medias names separated by a comma. Your available answer choices are: RDI, TVA Nouvelles, Le Devoir, La Presse, Radio-Canada (SRC), LCN, CTV, CBC, The Gazette, Journal de Québec, Journal de Montréal, Le Soleil, 98.5 FM, National Post, Globe and Mail, Global News, CNN, Fox News, Guardian. If the answer is not in these choices, ignore it: simply answer NA. Separate each media by a comma."
            ),
            list(
              "role" = "user",
              "content" = paste0("Identify the specific media outlet for the following response: ", media, ".")
            )
          )
        )
      }, timeout = 10)  # Timeout en secondes
    }, error = function(e) {
      cat("Erreur ou timeout pour la réponse :", i, "-", media, "\n")
      NULL  # En cas d'erreur ou de timeout, retourne NULL
    })
    
    end_time <- Sys.time()
    attempt_time <- as.numeric(end_time - start_time, units = "secs")
    
    if (!is.null(chat_prompt)) {
      request_successful <- TRUE
      cat("Réponse obtenue avec succès pour :", i, "-", media, "\n")
      message(paste0(media, " ==> ", chat_prompt[["choices"]][["message.content"]]))
      if (i == 1) {
        output <- chat_prompt[["choices"]][["message.content"]]
        names(output) <- i
      } else {
        output <- c(output, chat_prompt[["choices"]][["message.content"]])
        names(output)[i] <- i
      }
    } else {
      cat("Tentative échouée, nouvelle tentative pour :", i, "-", media, "\n")
      Sys.sleep(2)  # Petite pause avant de réessayer
    }
  }
}

april$O_C11_clean <- output

writexl::write_xlsx(april, "_SharedFolder_memoire-pot-growth/data/lake/omnibus/april/april_open_clean.xlsx")

# May ----------------------------------------------------------------

may <- readxl::read_excel("_SharedFolder_memoire-pot-growth/data/lake/omnibus/may/may_open.xlsx")

answers <- may$O_C11

for (i in 1:length(answers)) {
  media <- answers[i]
  request_successful <- FALSE
  attempt_time <- 0
  
  print(paste("Traitement de la réponse :", media))
  
  while (!request_successful && attempt_time <= 5) {
    start_time <- Sys.time()
    
    chat_prompt <- tryCatch({
      R.utils::withTimeout({
        create_chat_completion(
          model = "gpt-4",
          messages = list(
            list(
              "role" = "system",
              "content" = "You are a helpful assistant trained to assist in categorizing open answers about media sources. Only answer with the medias names separated by a comma. Your available answer choices are: RDI, TVA Nouvelles, Le Devoir, La Presse, Radio-Canada (SRC), LCN, CTV, CBC, The Gazette, Journal de Québec, Journal de Montréal, Le Soleil, 98.5 FM, National Post, Globe and Mail, Global News, CNN, Fox News, Guardian. If the answer is not in these choices, ignore it: simply answer NA. Separate each media by a comma."
            ),
            list(
              "role" = "user",
              "content" = paste0("Identify the specific media outlet for the following response: ", media, ".")
            )
          )
        )
      }, timeout = 10)  # Timeout en secondes
    }, error = function(e) {
      cat("Erreur ou timeout pour la réponse :", i, "-", media, "\n")
      NULL  # En cas d'erreur ou de timeout, retourne NULL
    })
    
    end_time <- Sys.time()
    attempt_time <- as.numeric(end_time - start_time, units = "secs")
    
    if (!is.null(chat_prompt)) {
      request_successful <- TRUE
      cat("Réponse obtenue avec succès pour :", i, "-", media, "\n")
      message(paste0(media, " ==> ", chat_prompt[["choices"]][["message.content"]]))
      if (i == 1) {
        output <- chat_prompt[["choices"]][["message.content"]]
      } else {
        output <- c(output, chat_prompt[["choices"]][["message.content"]])
      }
    } else {
      cat("Tentative échouée, nouvelle tentative pour :", i, "-", media, "\n")
      Sys.sleep(2)  # Petite pause avant de réessayer
    }
  }
}

may$O_C11_clean <- output

writexl::write_xlsx(may, "_SharedFolder_memoire-pot-growth/data/lake/omnibus/may/may_open_clean.xlsx")


# June ----------------------------------------------------------------

#june <- readxl::read_excel("_SharedFolder_memoire-pot-growth/data/lake/omnibus/june/june_open.xlsx")
#
#answers <- june$O_C11
#
#for (i in 1:length(answers)) {
#  media <- answers[i]
#  request_successful <- FALSE
#  attempt_time <- 0
#  
#  print(paste("Traitement de la réponse :", media))
#  
#  while (!request_successful && attempt_time <= 5) {
#    start_time <- Sys.time()
#    
#    chat_prompt <- tryCatch({
#      R.utils::withTimeout({
#        create_chat_completion(
#          model = "gpt-4",
#          messages = list(
#            list(
#              "role" = "system",
#              "content" = "You are a helpful assistant trained to assist in categorizing open answers about media sources. Only answer with the medias names separated by a comma. Your available answer choices are: RDI, TVA Nouvelles, Le Devoir, La Presse, Radio-Canada (SRC), LCN, CTV, CBC, The Gazette, Journal de Québec, Journal de Montréal, Le Soleil, 98.5 FM, National Post, Globe and Mail, Global News, CNN, Fox News, Guardian. If the answer is not in these choices, ignore it: simply answer NA. Separate each media by a comma."
#            ),
#            list(
#              "role" = "user",
#              "content" = paste0("Identify the specific media outlet for the following response: ", media, ".")
#            )
#          )
#        )
#      }, timeout = 10)  # Timeout en secondes
#    }, error = function(e) {
#      cat("Erreur ou timeout pour la réponse :", i, "-", media, "\n")
#      NULL  # En cas d'erreur ou de timeout, retourne NULL
#    })
#    
#    end_time <- Sys.time()
#    attempt_time <- as.numeric(end_time - start_time, units = "secs")
#    
#    if (!is.null(chat_prompt)) {
#      request_successful <- TRUE
#      cat("Réponse obtenue avec succès pour :", i, "-", media, "\n")
#      message(paste0(media, " ==> ", chat_prompt[["choices"]][["message.content"]]))
#      if (i == 1) {
#        output <- chat_prompt[["choices"]][["message.content"]]
#      } else {
#        output <- c(output, chat_prompt[["choices"]][["message.content"]])
#      }
#    } else {
#      cat("Tentative échouée, nouvelle tentative pour :", i, "-", media, "\n")
#      Sys.sleep(2)  # Petite pause avant de réessayer
#    }
#  }
#}
#
#june$O_C11_clean <- output
#
#writexl::write_xlsx(june, "_SharedFolder_memoire-pot-growth/data/lake/omnibus/june/june_open_clean.xlsx")
#
#