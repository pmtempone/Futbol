--tabla con datos por equipo y eventos

select even_id_evento,equipo_local as equipo,loc_goles as goles, loc_asistencias as asistencias,loc_disp_afuera as disp_afuera,
loc_disp_palo as disp_palo,loc_disp_atajado as disp_atajado,loc_penal_errado as penal_errado,loc_faltas as faltas,
loc_offsides as offsides,loc_amarillas as amarillas,loc_doble_ama as doble_amarilla,loc_despejes as despejes,
loc_quites as quites,loc_atajadas as atajadas,loc_ataj_penal as ataj_penal
into base_evento_equipo
from (select * from base_locales
union all
select * from base_visitantes) as a
order by 1,2;

--prueba de avg 5 partidos anteriores
SELECT even_id_evento, equipo,avg(goles) OVER w as lag_goles
FROM   base_evento_equipo
WINDOW w AS (PARTITION BY equipo
             ORDER BY base_evento_equipo
             ROWS BETWEEN 5 PRECEDING AND current row);