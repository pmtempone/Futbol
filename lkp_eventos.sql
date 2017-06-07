--armado de lkp con los eventos por fecha

select distinct fecha,even_id_evento,torneo,cate_nombre_categoria,"local.1" as local,fixt_local_goles,"visitante.1" as visitante,fixt_visitante_goles,victoria_l
into lkp_eventos
from base_original;

