# ========================================
# R/auth.R
# Sistema de autenticacion con usuarios, roles y Google Sheets
# ========================================
# Hoja esperada: "Usuarios" con columnas:
#   usuario | password_hash | rol | activo | creado_en | ultimo_login |
#   nombre_completo | correo
# Variables de entorno:
#   USERS_SHEET_ID  (ID de la Google Sheet; si NA, reusa google_sheets_id)
#   ADMIN_USER      (usuario admin semilla)
#   ADMIN_PASS      (password admin semilla; hasheado en el primer arranque)

AUTH_SHEET_NAME <- "Usuarios"

# ---- Helpers basicos ----

auth_users_sheet_id <- function() {
  id <- Sys.getenv("USERS_SHEET_ID", "")
  if (nchar(id) > 0) return(id)
  # Fallback: opcion global seteada en app.R (shiny::runApp aisla scope,
  # asi que no podemos confiar en .GlobalEnv siempre)
  gid <- getOption("rcualitext.sheet_id", NA_character_)
  if (!is.na(gid) && nchar(gid) > 0) return(gid)
  # Ultimo intento: buscar en GlobalEnv por si app.R lo expuso ahi
  if (exists("google_sheets_id", envir = .GlobalEnv, inherits = FALSE)) {
    g2 <- get("google_sheets_id", envir = .GlobalEnv)
    if (!is.null(g2) && nchar(g2) > 0) return(g2)
  }
  NA_character_
}

auth_hash_pwd <- function(pwd) {
  bcrypt::hashpw(pwd, bcrypt::gensalt(log_rounds = 12))
}

auth_verify_pwd <- function(pwd, hash) {
  tryCatch(isTRUE(bcrypt::checkpw(pwd, hash)), error = function(e) FALSE)
}

auth_generate_password <- function(n = 12) {
  chars <- c(letters, LETTERS, 0:9)
  paste(sample(chars, n, replace = TRUE), collapse = "")
}

# Crea la hoja Usuarios si no existe (idempotente). Requiere gs4_auth activo.
auth_ensure_users_sheet <- function() {
  sid <- auth_users_sheet_id()
  if (is.na(sid)) {
    message("auth: no sheet id available (USERS_SHEET_ID ni google_sheets_id)")
    return(FALSE)
  }
  tryCatch({
    sheets <- googlesheets4::sheet_names(sid)
    if (!AUTH_SHEET_NAME %in% sheets) {
      empty <- data.frame(
        usuario = character(), password_hash = character(),
        rol = character(), activo = logical(),
        creado_en = character(), ultimo_login = character(),
        nombre_completo = character(), correo = character(),
        stringsAsFactors = FALSE
      )
      googlesheets4::sheet_write(empty, ss = sid, sheet = AUTH_SHEET_NAME)
      message("auth: hoja '", AUTH_SHEET_NAME, "' creada")
    }
    TRUE
  }, error = function(e) {
    message("auth_ensure_users_sheet error: ", conditionMessage(e))
    FALSE
  })
}

# ---- Lectura ----

auth_read_all <- function() {
  sid <- auth_users_sheet_id()
  if (is.na(sid)) return(NULL)
  tryCatch({
    googlesheets4::read_sheet(sid, sheet = AUTH_SHEET_NAME,
                              col_types = "cccccccc")
  }, error = function(e) {
    message("auth_read_all error: ", conditionMessage(e))
    NULL
  })
}

auth_get_user <- function(usuario) {
  df <- auth_read_all()
  if (is.null(df) || nrow(df) == 0) return(NULL)
  row <- df[tolower(df$usuario) == tolower(usuario), , drop = FALSE]
  if (nrow(row) == 0) return(NULL)
  as.list(row[1, ])
}

auth_list_users <- function() {
  df <- auth_read_all()
  if (is.null(df)) return(data.frame())
  # No exponer el hash en el UI
  df[, setdiff(names(df), "password_hash"), drop = FALSE]
}

# ---- Escritura ----

auth_append_user <- function(usuario, password_hash, rol = "usuario",
                             nombre_completo = "", correo = "") {
  sid <- auth_users_sheet_id()
  if (is.na(sid)) stop("No sheet id")
  nueva <- data.frame(
    usuario = as.character(usuario),
    password_hash = as.character(password_hash),
    rol = as.character(rol),
    activo = "TRUE",
    creado_en = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    ultimo_login = "",
    nombre_completo = as.character(nombre_completo),
    correo = as.character(correo),
    stringsAsFactors = FALSE
  )
  googlesheets4::sheet_append(sid, nueva, sheet = AUTH_SHEET_NAME)
  invisible(TRUE)
}

auth_create_user <- function(usuario, password, rol = "usuario",
                             nombre_completo = "", correo = "") {
  usuario <- trimws(usuario)
  if (nchar(usuario) < 3) stop("Usuario debe tener al menos 3 caracteres")
  if (nchar(password) < 8) stop("Password debe tener al menos 8 caracteres")
  if (!rol %in% c("admin", "usuario")) stop("Rol invalido")
  if (!is.null(auth_get_user(usuario))) stop("Usuario ya existe: ", usuario)
  auth_append_user(usuario, auth_hash_pwd(password), rol,
                   nombre_completo, correo)
}

# Actualiza UNA fila en la hoja por usuario. `updates` es una lista nombrada.
auth_update_row <- function(usuario, updates) {
  sid <- auth_users_sheet_id()
  if (is.na(sid)) stop("No sheet id")
  df <- auth_read_all()
  if (is.null(df) || nrow(df) == 0) stop("Hoja vacia")
  idx <- which(tolower(df$usuario) == tolower(usuario))
  if (length(idx) == 0) stop("Usuario no encontrado: ", usuario)
  for (nm in names(updates)) {
    if (nm %in% names(df)) df[[nm]][idx] <- as.character(updates[[nm]])
  }
  # Reescribimos la hoja completa (simple y atomico)
  googlesheets4::sheet_write(df, ss = sid, sheet = AUTH_SHEET_NAME)
  invisible(TRUE)
}

auth_update_last_login <- function(usuario) {
  ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  tryCatch(auth_update_row(usuario, list(ultimo_login = ts)),
           error = function(e) invisible(FALSE))
}

auth_set_active <- function(usuario, active = TRUE) {
  auth_update_row(usuario, list(activo = as.character(isTRUE(active))))
}

auth_reset_password <- function(usuario, new_password = NULL) {
  pwd <- if (is.null(new_password)) auth_generate_password(12) else new_password
  if (nchar(pwd) < 8) stop("Password muy corto")
  auth_update_row(usuario, list(password_hash = auth_hash_pwd(pwd)))
  invisible(pwd)  # retorna la nueva contrasena en claro para mostrar UNA vez
}

auth_set_role <- function(usuario, rol) {
  if (!rol %in% c("admin", "usuario")) stop("Rol invalido")
  auth_update_row(usuario, list(rol = rol))
}

# Elimina permanentemente la fila del usuario en la hoja.
# No se puede deshacer (considerar auth_set_active() para reversible).
auth_delete_user <- function(usuario) {
  sid <- auth_users_sheet_id()
  if (is.na(sid)) stop("No sheet id")
  df <- auth_read_all()
  if (is.null(df) || nrow(df) == 0) stop("Hoja vacia")
  idx <- which(tolower(df$usuario) == tolower(usuario))
  if (length(idx) == 0) stop("Usuario no encontrado: ", usuario)
  df <- df[-idx, , drop = FALSE]
  googlesheets4::sheet_write(df, ss = sid, sheet = AUTH_SHEET_NAME)
  invisible(TRUE)
}

# ---- Autenticacion ----

# Resultado:
#   list(ok=TRUE, user=list(usuario=..., rol=..., ...)) si login correcto
#   list(ok=FALSE, reason="no_existe"|"inactivo"|"clave_incorrecta")
auth_login <- function(usuario, password) {
  u <- auth_get_user(usuario)
  if (is.null(u)) return(list(ok = FALSE, reason = "credenciales"))
  if (!isTRUE(as.logical(u$activo))) return(list(ok = FALSE, reason = "inactivo"))
  if (!auth_verify_pwd(password, u$password_hash)) {
    return(list(ok = FALSE, reason = "credenciales"))
  }
  auth_update_last_login(u$usuario)
  u$password_hash <- NULL  # nunca retornar el hash
  list(ok = TRUE, user = u)
}

# ---- Seed del admin ----

auth_seed_admin <- function() {
  admin_user <- Sys.getenv("ADMIN_USER", "")
  admin_pass <- Sys.getenv("ADMIN_PASS", "")
  if (nchar(admin_user) == 0 || nchar(admin_pass) == 0) {
    message("auth: ADMIN_USER/ADMIN_PASS no definidos; seed saltado")
    return(invisible(FALSE))
  }
  ok <- auth_ensure_users_sheet()
  if (!ok) return(invisible(FALSE))
  tryCatch({
    existing <- auth_get_user(admin_user)
    if (is.null(existing)) {
      auth_append_user(admin_user, auth_hash_pwd(admin_pass),
                       rol = "admin",
                       nombre_completo = "Administrador",
                       correo = "")
      message("auth: admin '", admin_user, "' creado (seed)")
    } else {
      # Ya existe: NO sobreescribimos el password (el admin ya pudo cambiarlo).
      # Solo aseguramos rol=admin y activo=TRUE para recuperacion de emergencia.
      if (existing$rol != "admin" || !isTRUE(as.logical(existing$activo))) {
        auth_update_row(admin_user, list(rol = "admin", activo = "TRUE"))
        message("auth: admin '", admin_user, "' restaurado a rol admin/activo")
      }
    }
    invisible(TRUE)
  }, error = function(e) {
    message("auth_seed_admin error: ", conditionMessage(e))
    invisible(FALSE)
  })
}
