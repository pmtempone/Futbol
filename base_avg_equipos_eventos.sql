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