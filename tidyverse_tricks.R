# Cargar paquetes
library(tidyverse)
library(datos)

## TRUCOS TIDYVERSE ----

## Truco 1: count() ----
paises %>% 
  count(continente, 
        name = "total_cont", sort = TRUE)

## Truco 2: count() con tres argumentos ----
paises %>% 
  count(continente, wt = poblacion,
        name = "total_pob", sort = TRUE)

## Truco 3: fct_reorder() + geom_col() + coord_flip() ----
paises %>% 
  count(continente, wt = poblacion,
        name = "total_pob", sort = TRUE) %>% 
  ggplot(aes(x = total_pob, 
             y = fct_reorder(continente, total_pob))) +
  geom_col() +
  labs(x = "población total",
       y = NULL)
  theme_minimal()

## Truco 4: separate() ----
df <- data.frame(period = c("Q1_y2019","Q2_y2019", "Q3_y2019","Q4_y2019"),
                 revenue = c(23,24,27,29))

df %>% 
  separate(period, 
           c("Cuartil","Año"), sep = "_y", remove = FALSE)

## Truco 5: extract() ----
df %>% 
  extract(period, 
          c("Cuartil","Año"), "Q(.*)_y(.*)")


# ---------------------------------------------------------------------#

a <- c("1","2","2")
b <- c("tasha","pablo","tasha")
c <- c("pablo","tasha","tasha")

base <- data.frame(a, b, c) %>% as.tibble()

# TRANFORMAR VARIABLES ----

## Reemplazar un único valor ----
base %>% 
  mutate(b = case_when(b == "tasha" ~ "ella", TRUE ~ b))

## Transformar el tipo de varias variables a la vez ----
base %>% 
  mutate_at(vars(b,c), as.factor)

base %>% 
  mutate(across(c(b,c),
                as.factor)
  )

# Transformar varias variables a la vez según una condición ----

### con {replace} ----
base %>% 
  mutate_at(vars(b,c),
            ~ replace(.,which(. == "pablo"), "hola"))

### con {case_when} ----
base %>% 
  mutate_at(vars(b,c),
            funs(case_when(. == "pablo" ~ "hola",
                           . == "tasha" ~ "tasha")))

# Transformar múltiples variables según múltiples condiciones ----
base %>% 
  mutate_at(vars(b,c),
            funs(case_when(. == "pablo" ~ "hola",    #1er cond
                           . == "tasha" ~ "adios"))) #2da cond

base %>% 
  mutate(across(b:c,
                ~ case_when(. == "pablo" ~ "hola",
                            TRUE ~ "other")
                )
         )

## Crear variable ID ----
base %>% 
  mutate(id = 1:nrow(base)) %>% 
  relocate(id, .before = a)


## Crear variables a partir de otras 

starwars %>% 
  mutate(new_group = case_when(
    homeworld == "Tatooine" & hair_color == "blond" ~ "yes",
    homeworld == "Tatooine" ~ "other tatooinian",
    TRUE ~ "no")) %>% 
  count(new_group, sort = TRUE, name = "frecuencia")


## Renombrar varias columnas a la vez ----

### de minúscula a mayúscula ----
flores %>% rename_all(toupper) %>% names()

### de mayúscula a minúscula ----
flores %>% rename_all(tolower) %>% names()

### con {rename_at} ----
flores %>% 
  rename_at(vars(Largo.Sepalo:Ancho.Petalo),
            ~(paste("var", 1:4, sep = "_"))) %>%
  names()

### con {rename_with} ----

flores %>% 
  rename_with(~ c("a","b","c","d"), ends_with("o")) %>% 
  names()

flores %>% 
  rename_with(~ paste("var", 1:4, sep = "_"), is.numeric) %>% 
  names()


# RESUMIR INFORMACIÓN ----

## summary ----
flores %>% 
  select(Largo.Sepalo, Largo.Petalo) %>% 
  summary()

## summarize ----
flores %>% 
  summarise(across(c(Largo.Sepalo,Largo.Petalo), 
                   c(mean,sd))
  )

## resumir y nombrar variables de resumen ----
flores %>% 
  summarise(across(c(Largo.Sepalo, Largo.Petalo),
            funs(media = mean, mediana = median))
            )

flores %>% 
  summarise_if(is.numeric, 
               funs(media = mean)
               )

flores %>% 
  summarise(across(ends_with("o"),
                   funs(media = mean))
            )

flores %>% 
  summarise(across(Largo.Sepalo:Largo.Petalo, funs(mean))
  )


# to allign ---------------------------------------------------------------
devtools::install_github("seasmith/AlignAssign")
# test
# list(surname = "Crichton",
#      firstName = "John",
#      address = NA)


# to compare --------------------------------------------------------------
df1 <- c(1:30) |> as.double()
df2 <- c(1:14,16,17:28,30) |> as.double()
waldo::compare(df1, df2)

df |> janitor::clean_names()


# to obtain frequencies ---------------------------------------------------

salud |> 
  janitor::tabyl(MES) |>  
  janitor::adorn_totals("row") |> 
  janitor::adorn_pct_formatting(2) 

salud |> 
  janitor::tabyl(MES,ESTRATO) |> 
  janitor::adorn_percentages("col") %>%
  janitor::adorn_pct_formatting(digits = 2) %>%
  janitor::adorn_ns()


# Extra ----

l <- letters[1:3]
n <- 1:3
df <- tibble(x = c(rep(l, 2), "x"))
df 

df %>% 
  mutate(y = n[match(x, l)]) 

mtcars <- mtcars %>% 
  rownames_to_column("model") 

mtcars %>% 
  mutate(make = word(model, 1))
