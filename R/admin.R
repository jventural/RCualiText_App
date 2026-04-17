# ========================================
# R/admin.R
# Panel de administracion: gestion de usuarios
# ========================================
# Solo accesible si rv$current_user$rol == "admin"

admin_tab_ui <- function() {
  shinydashboard::tabItem(
    tabName = "admin",
    fluidRow(
      column(12,
             h2(icon("user-shield"), " Administracion de usuarios"),
             helpText("Solo visible para administradores. Aqui puedes crear, editar y desactivar usuarios."),
             hr()
      )
    ),
    fluidRow(
      # ---- Panel crear usuario ----
      box(
        width = 5, title = "Crear nuevo usuario",
        status = "primary", solidHeader = TRUE, collapsible = TRUE,
        textInput("admin_new_usuario", "Usuario (login)",
                  placeholder = "ej: maria01"),
        textInput("admin_new_nombre", "Nombre completo"),
        textInput("admin_new_correo", "Correo electronico"),
        selectInput("admin_new_rol", "Rol",
                    choices = c("usuario", "admin"), selected = "usuario"),
        hr(),
        radioButtons("admin_new_pwd_mode", "Contrasena",
                     choices = c("Generar automatica (12 chars)" = "auto",
                                 "Definir manualmente" = "manual"),
                     selected = "auto"),
        conditionalPanel(
          condition = "input.admin_new_pwd_mode == 'manual'",
          passwordInput("admin_new_pwd", "Contrasena (min 8 chars)")
        ),
        actionButton("admin_btn_create", "Crear usuario",
                     icon = icon("user-plus"),
                     class = "btn-primary btn-block"),
        hr(),
        uiOutput("admin_create_result")
      ),
      # ---- Panel lista de usuarios ----
      box(
        width = 7, title = "Lista de usuarios",
        status = "success", solidHeader = TRUE, collapsible = TRUE,
        actionButton("admin_btn_refresh", "Recargar lista",
                     icon = icon("sync")),
        hr(),
        DT::DTOutput("admin_users_tbl"),
        hr(),
        h5("Acciones sobre usuario seleccionado"),
        fluidRow(
          column(6,
                 actionButton("admin_btn_toggle_active", "Activar / Desactivar",
                              icon = icon("power-off"),
                              class = "btn-warning btn-block")),
          column(6,
                 actionButton("admin_btn_reset_pwd", "Resetear contrasena",
                              icon = icon("key"),
                              class = "btn-danger btn-block"))
        ),
        br(),
        fluidRow(
          column(6,
                 selectInput("admin_change_rol", "Cambiar rol a",
                             choices = c("usuario", "admin"))),
          column(6,
                 actionButton("admin_btn_set_rol", "Aplicar rol",
                              icon = icon("user-tag"),
                              class = "btn-info btn-block"))
        ),
        hr(),
        uiOutput("admin_action_result")
      )
    ),
    fluidRow(
      # ---- Panel perfil propio ----
      box(
        width = 6, title = "Mi cuenta (admin)",
        status = "info", solidHeader = TRUE, collapsible = TRUE,
        verbatimTextOutput("admin_me"),
        hr(),
        h5("Cambiar mi contrasena"),
        passwordInput("admin_my_new_pwd", "Nueva contrasena (min 8 chars)"),
        passwordInput("admin_my_new_pwd_2", "Confirmar"),
        actionButton("admin_btn_change_my_pwd", "Cambiar",
                     icon = icon("key"), class = "btn-primary"),
        uiOutput("admin_my_pwd_result")
      ),
      # ---- Panel audit de logins ----
      box(
        width = 6, title = "Ultimos logins",
        status = "warning", solidHeader = TRUE, collapsible = TRUE,
        DT::DTOutput("admin_logins_tbl")
      )
    )
  )
}

setup_admin_server <- function(input, output, session, rv) {
  # Gate: si el usuario no es admin, no conectar observadores costosos
  # (la tab no debe renderizar igualmente)
  is_admin <- reactive({
    cu <- rv$current_user
    !is.null(cu) && isTRUE(cu$rol == "admin")
  })

  users_trigger <- reactiveVal(0)

  users_df <- reactive({
    users_trigger()  # dependencia manual para refresh
    req(is_admin())
    auth_list_users()
  })

  output$admin_users_tbl <- DT::renderDT({
    df <- users_df()
    if (is.null(df) || nrow(df) == 0) {
      return(DT::datatable(data.frame(Info = "No hay usuarios"),
                           rownames = FALSE, options = list(dom = "t")))
    }
    DT::datatable(df, rownames = FALSE, selection = "single",
                  options = list(pageLength = 10, scrollX = TRUE))
  })

  observeEvent(input$admin_btn_refresh, {
    users_trigger(users_trigger() + 1)
  })

  # ---- Crear usuario ----
  observeEvent(input$admin_btn_create, {
    req(is_admin())
    usuario <- trimws(input$admin_new_usuario %||% "")
    if (nchar(usuario) < 3) {
      output$admin_create_result <- renderUI(
        div(class = "alert alert-danger",
            "Usuario debe tener minimo 3 caracteres"))
      return()
    }
    pwd_mode <- input$admin_new_pwd_mode
    if (pwd_mode == "manual") {
      pwd <- input$admin_new_pwd %||% ""
      if (nchar(pwd) < 8) {
        output$admin_create_result <- renderUI(
          div(class = "alert alert-danger",
              "Contrasena debe tener minimo 8 caracteres"))
        return()
      }
    } else {
      pwd <- auth_generate_password(12)
    }
    tryCatch({
      auth_create_user(
        usuario = usuario,
        password = pwd,
        rol = input$admin_new_rol %||% "usuario",
        nombre_completo = input$admin_new_nombre %||% "",
        correo = input$admin_new_correo %||% ""
      )
      users_trigger(users_trigger() + 1)
      output$admin_create_result <- renderUI(
        tagList(
          div(class = "alert alert-success",
              icon("check-circle"), " Usuario creado correctamente."),
          div(class = "well",
              style = "background: #fff9db; border: 2px solid #f39c12;",
              h4(icon("exclamation-triangle"),
                 " GUARDA ESTAS CREDENCIALES AHORA"),
              tags$p("No se volveran a mostrar. Enviaselas al usuario por un canal seguro."),
              tags$pre(style = "font-size: 14px; background: #fff; padding: 10px;",
                       paste0("Usuario:     ", usuario, "\n",
                              "Contrasena:  ", pwd)))))
      # Limpiar campos
      updateTextInput(session, "admin_new_usuario", value = "")
      updateTextInput(session, "admin_new_nombre", value = "")
      updateTextInput(session, "admin_new_correo", value = "")
      if (pwd_mode == "manual") {
        updateTextInput(session, "admin_new_pwd", value = "")
      }
    }, error = function(e) {
      output$admin_create_result <- renderUI(
        div(class = "alert alert-danger", conditionMessage(e)))
    })
  })

  # Helper: obtener usuario seleccionado en la tabla
  selected_user <- reactive({
    sel <- input$admin_users_tbl_rows_selected
    if (length(sel) == 0) return(NULL)
    df <- users_df()
    if (is.null(df) || nrow(df) < sel) return(NULL)
    df$usuario[sel]
  })

  # ---- Activar/Desactivar ----
  observeEvent(input$admin_btn_toggle_active, {
    req(is_admin())
    usr <- selected_user()
    if (is.null(usr)) {
      output$admin_action_result <- renderUI(
        div(class = "alert alert-warning",
            "Selecciona primero un usuario en la tabla"))
      return()
    }
    if (identical(tolower(usr), tolower(rv$current_user$usuario))) {
      output$admin_action_result <- renderUI(
        div(class = "alert alert-danger",
            "No puedes desactivarte a ti mismo"))
      return()
    }
    tryCatch({
      df <- users_df()
      cur <- as.logical(df$activo[df$usuario == usr])
      auth_set_active(usr, !isTRUE(cur))
      users_trigger(users_trigger() + 1)
      output$admin_action_result <- renderUI(
        div(class = "alert alert-success",
            paste0("Usuario '", usr, "' ahora esta ",
                   ifelse(!isTRUE(cur), "ACTIVO", "INACTIVO"))))
    }, error = function(e) {
      output$admin_action_result <- renderUI(
        div(class = "alert alert-danger", conditionMessage(e)))
    })
  })

  # ---- Reset password ----
  observeEvent(input$admin_btn_reset_pwd, {
    req(is_admin())
    usr <- selected_user()
    if (is.null(usr)) {
      output$admin_action_result <- renderUI(
        div(class = "alert alert-warning",
            "Selecciona primero un usuario en la tabla"))
      return()
    }
    tryCatch({
      new_pwd <- auth_reset_password(usr)
      output$admin_action_result <- renderUI(
        div(class = "well",
            style = "background: #fff9db; border: 2px solid #e67e22;",
            h4(icon("exclamation-triangle"),
               " Nueva contrasena para '", usr, "'"),
            tags$p("Esta contrasena NO se volvera a mostrar. Guardala."),
            tags$pre(style = "font-size: 14px; background: #fff; padding: 10px;",
                     new_pwd)))
    }, error = function(e) {
      output$admin_action_result <- renderUI(
        div(class = "alert alert-danger", conditionMessage(e)))
    })
  })

  # ---- Cambiar rol ----
  observeEvent(input$admin_btn_set_rol, {
    req(is_admin())
    usr <- selected_user()
    if (is.null(usr)) {
      output$admin_action_result <- renderUI(
        div(class = "alert alert-warning",
            "Selecciona primero un usuario en la tabla"))
      return()
    }
    if (identical(tolower(usr), tolower(rv$current_user$usuario)) &&
        input$admin_change_rol != "admin") {
      output$admin_action_result <- renderUI(
        div(class = "alert alert-danger",
            "No puedes quitarte el rol de admin a ti mismo"))
      return()
    }
    tryCatch({
      auth_set_role(usr, input$admin_change_rol)
      users_trigger(users_trigger() + 1)
      output$admin_action_result <- renderUI(
        div(class = "alert alert-success",
            paste0("Rol de '", usr, "' cambiado a: ",
                   input$admin_change_rol)))
    }, error = function(e) {
      output$admin_action_result <- renderUI(
        div(class = "alert alert-danger", conditionMessage(e)))
    })
  })

  # ---- Cambiar mi propia contrasena ----
  output$admin_me <- renderPrint({
    cu <- rv$current_user
    if (is.null(cu)) return(cat("(sin sesion)"))
    cat("Usuario:", cu$usuario, "\n")
    cat("Rol:", cu$rol, "\n")
    cat("Nombre:", cu$nombre_completo %||% "", "\n")
    cat("Correo:", cu$correo %||% "", "\n")
  })

  observeEvent(input$admin_btn_change_my_pwd, {
    req(is_admin())
    p1 <- input$admin_my_new_pwd %||% ""
    p2 <- input$admin_my_new_pwd_2 %||% ""
    if (nchar(p1) < 8) {
      output$admin_my_pwd_result <- renderUI(
        div(class = "alert alert-danger",
            "Contrasena debe tener minimo 8 caracteres")); return()
    }
    if (p1 != p2) {
      output$admin_my_pwd_result <- renderUI(
        div(class = "alert alert-danger",
            "Las contrasenas no coinciden")); return()
    }
    tryCatch({
      auth_reset_password(rv$current_user$usuario, new_password = p1)
      updateTextInput(session, "admin_my_new_pwd", value = "")
      updateTextInput(session, "admin_my_new_pwd_2", value = "")
      output$admin_my_pwd_result <- renderUI(
        div(class = "alert alert-success",
            icon("check"), " Contrasena actualizada. ",
            "La proxima vez que entres usa la nueva."))
    }, error = function(e) {
      output$admin_my_pwd_result <- renderUI(
        div(class = "alert alert-danger", conditionMessage(e)))
    })
  })

  # ---- Audit de logins ----
  output$admin_logins_tbl <- DT::renderDT({
    req(is_admin())
    df <- users_df()
    if (is.null(df) || nrow(df) == 0) {
      return(DT::datatable(data.frame(Info = "No hay usuarios"),
                           rownames = FALSE, options = list(dom = "t")))
    }
    df2 <- df[, intersect(c("usuario", "rol", "activo", "ultimo_login",
                            "creado_en"), names(df))]
    df2 <- df2[order(df2$ultimo_login, decreasing = TRUE), ]
    DT::datatable(df2, rownames = FALSE,
                  options = list(pageLength = 10, scrollX = TRUE))
  })
}
