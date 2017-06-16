--armado de lkp con los eventos por fecha

select distinct fecha,even_id_evento,torneo,cate_nombre_categoria,"local.1" as local,fixt_local_goles,"visitante.1" as visitante,fixt_visitante_goles,victoria_l
into lkp_eventos
from base_original;


--base de goles y resultado
select * 
into fct_resultados_equipos
from 
(select even_id_evento, local as equipo, fixt_local_goles as goles_realizados,fixt_visitante_goles as goles_recibidos, 
case when fixt_local_goles > fixt_visitante_goles then 'V'
when fixt_local_goles=fixt_visitante_goles then 'E'
when fixt_local_goles<fixt_visitante_goles then 'D'
end as resultado
from lkp_eventos
union
select even_id_evento, visitante as equipo,fixt_visitante_goles as goles_realizados, fixt_local_goles as goles_recibidos,
case when fixt_local_goles < fixt_visitante_goles then 'V'
when fixt_local_goles=fixt_visitante_goles then 'E'
when fixt_local_goles>fixt_visitante_goles then 'D'
end as resultado
from lkp_eventos
order by even_id_evento) as a
