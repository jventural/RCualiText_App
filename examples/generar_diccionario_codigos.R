# Diccionario de codigos para Analisis IA > Code Dictionary
# ALINEADO con los 6 codigos del proyecto_demo_tutorial.rds:
#   Motivacion, Metodologia, Asesor, Tiempo, Apoyo emocional, Obstaculos
# en 3 categorias: Factores personales, institucionales, sociales.

library(openxlsx)

OUT_DIR <- "D:/16_Shinys/RCualiText_App/examples"

dic <- data.frame(
  category = c(
    # Factores personales (3)
    "Factores personales",
    "Factores personales",
    "Factores personales",
    # Factores institucionales (2)
    "Factores institucionales",
    "Factores institucionales",
    # Factores sociales (1)
    "Factores sociales"
  ),
  code = c(
    "Motivacion",
    "Tiempo",
    "Obstaculos",
    "Metodologia",
    "Asesor",
    "Apoyo emocional"
  ),
  definition = c(
    # --- Factores personales ---
    "Razones internas, pasion, vocacion o intereses personales del tesista que lo impulsan a realizar la investigacion. Incluye expresiones sobre que le apasiona el tema, se identifica con el problema, quiere aportar a su campo, o tiene una motivacion etica. Ejemplo: 'elegi este tema porque me duele, tengo familia en zonas rurales sin agua'.",
    "Descripciones sobre como el tesista administra, planifica, pierde o sacrifica el tiempo para avanzar la tesis. Incluye: jornadas dobles, trabajo nocturno, falta de horas, carga horaria, sacrificios de ocio/gimnasio/amigos, percepcion de plazos apretados, cansancio por exceso de horas dedicadas. Ejemplo: 'la tesis la hago de noche y los domingos', 'he vivido a punta de cafe este ultimo anio'.",
    "Menciones de obstaculos, barreras o dificultades concretas en el proceso: bloqueos de escritura, salud mental afectada, problemas tecnicos (grabadora, laptop), crisis de abandono, procrastinacion, parálisis por perfeccionismo, comparacion con otros, falta de autoeficacia, problemas de muestreo, rechazo institucional. Ejemplo: 'estoy paralizada, cada vez que abro Word borro lo que escribo', 'la empresa me cancelo el convenio'.",
    # --- Factores institucionales ---
    "Referencias a aspectos metodologicos de la investigacion: diseno de estudio, muestreo, instrumentos, analisis de datos (SPSS, software cuantitativo o cualitativo), validez, codificacion, entrevistas, grupos focales, saturacion teorica, triangulacion, calidad del rigor cientifico. Ejemplo: 'estoy en analisis de datos, apliqué un instrumento a 312 universitarios', 'mi tesis es cualitativa, llevo 7 entrevistas'.",
    "Descripciones sobre la figura y rol del asesor/a: disponibilidad, estilo de supervision, feedback, exigencia, relacion de trabajo, conflictos o satisfaccion con el acompanamiento. Incluye asesor distante, exigente, sobreinvolucrado, comprometido, ausente, critico o paciente. Ejemplo: 'el doctor Mendoza es duro pero me esta formando', 'mi asesora me llama los domingos a las 8 de la manana'.",
    # --- Factores sociales ---
    "Soporte emocional, consejos o ayuda practica recibidos de red social cercana: familia (padres, hermanos), pareja/conyuge, companeros tesistas, amigos, grupos de WhatsApp, psicologos o psiquiatras. Tambien incluye sensacion de aislamiento academico o soledad por falta de esta red. Ejemplo: 'mi papa me dijo no abandones', 'un grupo de Facebook de tesistas con TDAH me cambio la perspectiva'."
  ),
  stringsAsFactors = FALSE
)

# Verificar consistencia con los codigos del demo
codigos_esperados <- c("Motivacion", "Metodologia", "Asesor",
                       "Tiempo", "Apoyo emocional", "Obstaculos")
stopifnot(setequal(dic$code, codigos_esperados))
stopifnot(!any(duplicated(dic$code)))

# Exportar xlsx con formato
wb <- createWorkbook()
addWorksheet(wb, "Diccionario")
writeData(wb, "Diccionario", dic)

# Estilo header
hs <- createStyle(
  fontColour = "#FFFFFF", fgFill = "#2980b9",
  halign = "center", textDecoration = "bold",
  border = "Bottom", borderStyle = "medium"
)
addStyle(wb, "Diccionario", hs, rows = 1, cols = 1:3, gridExpand = TRUE)

# Colores de fondo por categoria (misma logica que CAT_PALETTE pero suavizados)
cat_colors <- c(
  "Factores personales"      = "#FDEDEC",
  "Factores institucionales" = "#F4ECF7",
  "Factores sociales"        = "#E8F8F0"
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

setColWidths(wb, "Diccionario", cols = 1, widths = 28)
setColWidths(wb, "Diccionario", cols = 2, widths = 22)
setColWidths(wb, "Diccionario", cols = 3, widths = 110)
freezePane(wb, "Diccionario", firstActiveRow = 2)

xlsx_path <- file.path(OUT_DIR, "diccionario_codigos_tesistas.xlsx")
saveWorkbook(wb, xlsx_path, overwrite = TRUE)

# CSV con BOM UTF-8
csv_path <- file.path(OUT_DIR, "diccionario_codigos_tesistas.csv")
con <- file(csv_path, open = "wb")
writeBin(charToRaw("\ufeff"), con)
close(con)
write.table(dic, csv_path, sep = ",", row.names = FALSE,
            fileEncoding = "UTF-8", append = TRUE, col.names = TRUE)

cat("Diccionario ALINEADO con el .rds demo:\n")
cat("  ", xlsx_path, "\n")
cat("  ", csv_path, "\n")
cat("\n", nrow(dic), "codigos en", length(unique(dic$category)), "categorias:\n")
for (c in unique(dic$category)) {
  codes <- dic$code[dic$category == c]
  cat(sprintf("  - %s: %s\n", c, paste(codes, collapse = ", ")))
}
