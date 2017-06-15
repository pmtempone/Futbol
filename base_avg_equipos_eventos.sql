--base avg de eventos equipos

SELECT even_id_evento, equipo,avg(goles) OVER w as avg_goles,
avg(asistencias) OVER w as avg_asistencias,
avg(disp_afuera) OVER w as avg_disp_afuera,
avg(disp_palo) OVER w as avg_disp_palo,
avg(disp_atajado) OVER w as avg_disp_atajado,
avg(penal_errado) OVER w as avg_penal_errado,
avg(faltas) OVER w as avg_faltas,
avg(offsides) OVER w as avg_offsides,
avg(amarillas) OVER w as avg_amarillas,
avg(doble_amarilla) OVER w as avg_doble_amarilla,
avg(despejes) OVER w as avg_despejes,
avg(quites) OVER w as avg_quites,
avg(atajadas) OVER w as avg_atajadas,
avg(ataj_penal) OVER w as avg_ataj_penal
into base_avg_equipos_eventos
FROM   base_evento_equipo
WINDOW w AS (PARTITION BY equipo
             ORDER BY base_evento_equipo
             ROWS BETWEEN 6 PRECEDING AND 1 PRECEDING);
             
--base sum de eventos equipos

SELECT even_id_evento, equipo,sum(goles) OVER w as sum_goles,
sum(asistencias) OVER w as sum_asistencias,
sum(disp_afuera) OVER w as sum_disp_afuera,
sum(disp_palo) OVER w as sum_disp_palo,
sum(disp_atajado) OVER w as sum_disp_atajado,
sum(penal_errado) OVER w as sum_penal_errado,
sum(faltas) OVER w as sum_faltas,
sum(offsides) OVER w as sum_offsides,
sum(amarillas) OVER w as sum_amarillas,
sum(doble_amarilla) OVER w as sum_doble_amarilla,
sum(despejes) OVER w as sum_despejes,
sum(quites) OVER w as sum_quites,
sum(atajadas) OVER w as sum_atajadas,
sum(ataj_penal) OVER w as sum_ataj_penal
into base_sum_equipos_eventos
FROM   base_evento_equipo
WINDOW w AS (PARTITION BY equipo
             ORDER BY base_evento_equipo
             ROWS BETWEEN 6 PRECEDING AND 1 PRECEDING);
             
--base max de eventos equipos

SELECT even_id_evento, equipo,max(goles) OVER w as max_goles,
max(asistencias) OVER w as max_asistencias,
max(disp_afuera) OVER w as max_disp_afuera,
max(disp_palo) OVER w as max_disp_palo,
max(disp_atajado) OVER w as max_disp_atajado,
max(penal_errado) OVER w as max_penal_errado,
max(faltas) OVER w as max_faltas,
max(offsides) OVER w as max_offsides,
max(amarillas) OVER w as max_amarillas,
max(doble_amarilla) OVER w as max_doble_amarilla,
max(despejes) OVER w as max_despejes,
max(quites) OVER w as max_quites,
max(atajadas) OVER w as max_atajadas,
max(ataj_penal) OVER w as max_ataj_penal
into base_max_equipos_eventos
FROM   base_evento_equipo
WINDOW w AS (PARTITION BY equipo
             ORDER BY base_evento_equipo
             ROWS BETWEEN 6 PRECEDING AND 1 PRECEDING);
             
--base min de eventos equipos

SELECT even_id_evento, equipo,min(goles) OVER w as min_goles,
min(asistencias) OVER w as min_asistencias,
min(disp_afuera) OVER w as min_disp_afuera,
min(disp_palo) OVER w as min_disp_palo,
min(disp_atajado) OVER w as min_disp_atajado,
min(penal_errado) OVER w as min_penal_errado,
min(faltas) OVER w as min_faltas,
min(offsides) OVER w as min_offsides,
min(amarillas) OVER w as min_amarillas,
min(doble_amarilla) OVER w as min_doble_amarilla,
min(despejes) OVER w as min_despejes,
min(quites) OVER w as min_quites,
min(atajadas) OVER w as min_atajadas,
min(ataj_penal) OVER w as min_ataj_penal
into base_min_equipos_eventos
FROM   base_evento_equipo
WINDOW w AS (PARTITION BY equipo
             ORDER BY base_evento_equipo
             ROWS BETWEEN 6 PRECEDING AND 1 PRECEDING);
             
             
