# Diccionario de codigos para subir en Analisis IA > Code Dictionary
# Tema: experiencia de tesistas universitarios (entrevistas del demo)
# Columnas requeridas por RCualiText: category, code, definition

library(openxlsx)

OUT_DIR <- "D:/16_Shinys/RCualiText_App/examples"

dic <- data.frame(
  category = c(
    rep("Factores personales", 7),
    rep("Factores institucionales", 6),
    rep("Factores sociales", 5),
    rep("Factores economicos y laborales", 3),
    rep("Experiencias emocionales", 5)
  ),
  code = c(
    # --- Factores personales ---
    "motivacion_intrinseca",
    "autoeficacia",
    "autoexigencia_perfeccionismo",
    "gestion_del_tiempo",
    "salud_mental",
    "estrategias_afrontamiento",
    "procrastinacion_evitacion",
    # --- Factores institucionales ---
    "asesor_distante",
    "asesor_exigente",
    "asesor_sobreinvolucrado",
    "metodologia_investigacion",
    "burocracia_universitaria",
    "recursos_universidad",
    # --- Factores sociales ---
    "apoyo_familiar",
    "apoyo_pareja",
    "grupo_pares_tesistas",
    "comparacion_social",
    "aislamiento_academico",
    # --- Factores economicos y laborales ---
    "trabajo_paralelo",
    "sacrificio_economico",
    "necesidad_financiamiento",
    # --- Experiencias emocionales ---
    "crisis_abandono",
    "logro_avance",
    "proceso_transformador",
    "emocion_negativa",
    "emocion_positiva"
  ),
  definition = c(
    # --- Factores personales ---
    "Razones internas, pasion o intereses personales del tesista que lo impulsan a realizar la tesis (ej: tema que le duele, vocacion, conviccion etica).",
    "Creencia del tesista sobre su propia capacidad para completar la tesis; confianza o inseguridad frente a los retos academicos.",
    "Exigencia interna desproporcionada, estandares perfeccionistas que bloquean el avance o generan insatisfaccion con el propio trabajo.",
    "Descripciones sobre como el tesista organiza, planifica o falla en administrar el tiempo disponible para la tesis.",
    "Menciones de ansiedad, depresion, burnout, insomnio, atencion psicologica o psiquiatrica, trastornos o sintomas mentales atribuidos al proceso.",
    "Tacticas, rutinas o estrategias concretas que el tesista aplica para manejar dificultades (ej: Pomodoro, escribir mal, dividir tareas).",
    "Conductas de postergacion, evitacion del trabajo, uso de distractores (Netflix, redes) para no enfrentar la tesis."
  ,
    # --- Factores institucionales ---
    "Descripciones del asesor como poco disponible, con respuestas tardias, falta de feedback sustantivo o ausencia de seguimiento.",
    "Asesor descrito como duro, demandante, con altas expectativas y retroalimentacion critica pero comprometido con la formacion.",
    "Asesor con presencia excesiva, intrusiva, demandas fuera de horario, vigilancia constante del avance o perdida de autonomia del tesista.",
    "Referencias a aspectos metodologicos de la investigacion: diseno, muestreo, instrumentos, analisis de datos, validez, rigor cientifico.",
    "Tramites, requisitos, reglamentos, plazos institucionales que generan friccion o retrasos al proceso de tesis.",
    "Servicios de la universidad que el tesista uso o valoro (biblioteca, psicologia universitaria, talleres, asesorias metodologicas, plataformas).",
    # --- Factores sociales ---
    "Soporte emocional, consejos o ayuda practica de familiares (padres, hermanos) durante el proceso de tesis.",
    "Apoyo, paciencia o tension con la pareja/conyugue derivada de la dedicacion a la tesis (incluidas rupturas por la tesis).",
    "Relaciones con otros tesistas (grupos de estudio, WhatsApp, amistades academicas) que sirven como soporte emocional o tecnico.",
    "Menciones donde el tesista se compara con companeros (ritmo, logros, publicaciones) generando ansiedad, envidia o sensacion de retraso.",
    "Percepcion de soledad durante el proceso, falta de red de apoyo, encierro social, sensacion de estar solo frente al trabajo.",
    # --- Factores economicos y laborales ---
    "Compatibilizacion entre trabajo remunerado y tesis, incluyendo cansancio, jornada doble, horarios restringidos.",
    "Gastos asociados a la tesis (estadistico, impresiones, conferencias, datos) y el esfuerzo economico que implican.",
    "Necesidad de financiamiento, becas, apoyo economico para poder continuar; preocupaciones economicas del tesista.",
    # --- Experiencias emocionales ---
    "Momentos concretos en que el tesista penso en abandonar, considero retirar la tesis, tuvo crisis de desmotivacion aguda o quiso rendirse.",
    "Satisfaccion por avances concretos: capitulo terminado, aprobacion del tribunal, defensa exitosa, reconocimiento del asesor.",
    "Reflexiones sobre como el proceso cambio al tesista: aprendizajes personales, cambio de perspectiva, habilidades nuevas, madurez.",
    "Expresiones de tristeza, frustracion, rabia, impotencia, miedo, culpa, agotamiento vinculadas al proceso de tesis.",
    "Expresiones de alegria, orgullo, esperanza, gratitud, alivio asociadas con momentos o personas durante la tesis."
  ),
  stringsAsFactors = FALSE
)

# Verificar consistencia
stopifnot(length(dic$category) == length(dic$code))
stopifnot(length(dic$code) == length(dic$definition))
stopifnot(!any(duplicated(dic$code)))

# Exportar xlsx con formato
wb <- createWorkbook()
addWorksheet(wb, "Diccionario")
writeData(wb, "Diccionario", dic)

# Formato header
hs <- createStyle(
  fontColour = "#FFFFFF", fgFill = "#2980b9",
  halign = "center", textDecoration = "bold",
  border = "Bottom", borderStyle = "medium"
)
addStyle(wb, "Diccionario", hs, rows = 1, cols = 1:3, gridExpand = TRUE)

# Color por categoria (consistente con CAT_PALETTE de la app)
cat_colors <- c(
  "Factores personales"             = "#FDEDEC",
  "Factores institucionales"        = "#F4ECF7",
  "Factores sociales"               = "#E8F8F0",
  "Factores economicos y laborales" = "#FEF5E7",
  "Experiencias emocionales"        = "#EAF2F8"
)
for (i in seq_len(nrow(dic))) {
  col <- cat_colors[dic$category[i]]
  if (!is.na(col)) {
    rowStyle <- createStyle(fgFill = col, wrapText = TRUE,
                            valign = "top", halign = "left")
    addStyle(wb, "Diccionario", rowStyle,
             rows = i + 1, cols = 1:3, gridExpand = TRUE)
  }
}

# Anchos de columna
setColWidths(wb, "Diccionario", cols = 1, widths = 32)
setColWidths(wb, "Diccionario", cols = 2, widths = 32)
setColWidths(wb, "Diccionario", cols = 3, widths = 90)

# Freeze header
freezePane(wb, "Diccionario", firstActiveRow = 2)

xlsx_path <- file.path(OUT_DIR, "diccionario_codigos_tesistas.xlsx")
saveWorkbook(wb, xlsx_path, overwrite = TRUE)

# Version CSV tambien (UTF-8 con BOM para Excel en Windows)
csv_path <- file.path(OUT_DIR, "diccionario_codigos_tesistas.csv")
con <- file(csv_path, open = "wb")
writeBin(charToRaw("\ufeff"), con)  # BOM
close(con)
write.table(dic, csv_path, sep = ",", row.names = FALSE,
            fileEncoding = "UTF-8", append = TRUE, col.names = TRUE)

cat("Diccionario generado:\n")
cat(" -", xlsx_path, "(", nrow(dic), "codigos en", length(unique(dic$category)), "categorias)\n")
cat(" -", csv_path, "\n")
cat("\nDistribucion por categoria:\n")
print(table(dic$category))
