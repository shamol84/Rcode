send.mail(from = "sdl43@case.edu",
          to = "mah228@case.edu",
          subject = "email check",
          body = "Body of the email",
          smtp = list(host.name = "smtp.gmail.com", port = 465, user.name = "sdl43@case.edu", passwd = "Pho1ton1", ssl = TRUE),
          authenticate = TRUE,
          send = TRUE)