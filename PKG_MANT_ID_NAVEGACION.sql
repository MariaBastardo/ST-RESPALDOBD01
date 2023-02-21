CREATE OR REPLACE PACKAGE PKG_MANT_ID_NAVEGACION
AS
  
 --Proyecto (Innobyte): Simulador de Ofertas (Universidad Santo Tomas)
    PROCEDURE P1_INSERTAR_PER_USU (iRut in ta_sim_per_persona.per_rut%TYPE, iNombre in  ta_sim_per_persona.per_nombre%TYPE,iemail in  ta_sim_per_persona.per_email%TYPE,
                                    Ok out Number,oErr out Number,oMsg out varchar2 );
    PROCEDURE P2_LISTAR_PER_USU_ROL;
    PROCEDURE P3_ELIMINAR_USU_ROL (iUsuId in ta_sim_usu_usuario.usu_id%TYPE,Ok out Number,oErr out Number,oMsg out varchar2  ); 
    PROCEDURE P4_INSERTAR_ROL  (iRol_Nombre in ta_sim_rol_rol.rol_nombre%TYPE , iRol_Ins_Cod in tam_sim_ins_institucion.Ins_Cod%TYPE,irol_flg_ciu in ta_sim_rol_rol. rol_flg_ciu %type,
                                irol_flg_sed in ta_sim_rol_rol. rol_flg_sed %type, irol_flg_fca in ta_sim_rol_rol. rol_flg_fca %type, irol_flg_cdis in ta_sim_rol_rol. rol_flg_cdis%type,
                                irol_flg_car in ta_sim_rol_rol. rol_flg_car%type
                                ,Ok out Number,oErr out Number,oMsg out varchar2 );
    PROCEDURE P5_ASOCIAR_USU_ROL (iUsuId in ta_sim_usu_usuario.usu_id%TYPE, iRolid in ta_sim_rol_rol.rol_id%TYPE ,Ok out Number,oErr out Number,oMsg out varchar2 );
    PROCEDURE P6_LISTAR_ROLES;
    PROCEDURE P7_LISTAR_USU_ROL(iUsuId in ta_sim_usu_usuario.usu_id%TYPE);
    PROCEDURE P8_ACTUALIZAR_ROL (iRol_Nombre in ta_sim_rol_rol.rol_nombre%TYPE, iRol_Ins_Cod in ta_sim_rol_rol.ROL_INS_COD%TYPE, iRol_id in ta_sim_rol_rol.rol_id%TYPE
                                  ,Ok out Number,oErr out Number,oMsg out varchar2);
    PROCEDURE P9_LOGIN_PERSONA_SEL (iRut in ta_sim_per_persona.per_rut%TYPE
                                   ,oPer_Id out  ta_sim_per_persona.per_id%TYPE, oRut out ta_sim_per_persona.per_rut%TYPE
                                   ,oNombre out ta_sim_per_persona.per_nombre%TYPE,  oEmail out  ta_sim_per_persona.per_email%TYPE
                                   ,Ok out Number,oErr out Number,oMsg out varchar2);
     PROCEDURE P10_SIM_PRR_SEL  (iRol in ta_sim_rol_rol.rol_id%TYPE);                                
     PROCEDURE P11_LISTAR_USU_ROL_CRG (iUsuId in ta_sim_usu_usuario.usu_id%TYPE);
     PROCEDURE P12_CARGAR_JSON (iUsuId in ta_sim_usu_usuario.usu_id%TYPE, iData in CLOB, Ok out Number,oErr out Number,oMsg out varchar2 );
     PROCEDURE LISTAR_CRG_USU_JSON;
  --Log de errores
    PROCEDURE PRC_LOG_ERROR(iLOG_SRC varchar2, iLOG_FUNCION varchar2, iLOG_MSG varchar2);
END PKG_MANT_ID_NAVEGACION;
/


CREATE OR REPLACE PACKAGE BODY PKG_MANT_ID_NAVEGACION
AS

---PRC_LOG_ERROR: Procedimiento generico para almacenar los errores 
    PROCEDURE PRC_LOG_ERROR(iLOG_SRC varchar2, iLOG_FUNCION varchar2, iLOG_MSG varchar2) AS
      nlog_id number;
    begin 
           --secuencia de log
           select nvl(max(log_id),0) into nlog_id
             from ta_sim_log_log;
             
           --inserta error en tabla
           Insert into ta_sim_log_log (LOG_ID,LGT_ID,LOG_SRC,LOG_FUNCION,LOG_MSG,LOG_FCH_REGISTRO)
                  values (nlog_id + 1,null,iLOG_SRC,iLOG_FUNCION,iLOG_MSG,to_char(sysdate, 'dd-mm-yyyy HH24:MI:SS'));
           commit;
    end;

--- P1_INSERTAR_PER_USU: procedimiento que inserta persona y usuario
  PROCEDURE P1_INSERTAR_PER_USU (iRut in ta_sim_per_persona.per_rut%TYPE, iNombre in  ta_sim_per_persona.per_nombre%TYPE,iemail in  ta_sim_per_persona.per_email%TYPE,
                                  Ok out Number,oErr out Number,oMsg out varchar2 ) AS
     nMaxPer number;
     nMaxUsu number;
     vReg varchar2(1);
     vReg1 varchar2(1);
     vUser varchar2(1);
     nlog_id number;
     vMsgErr varchar2(500);
     nPer_id number;

	BEGIN
       --Secuencia persona
       Begin
        select nvl(max(per_id),0) into nMaxPer
        from ta_sim_per_persona;
         EXCEPTION
            WHEN OTHERS THEN 
            Ok := 0;
            oErr := 1;
            oMsg :=  'Error en genera secuencia persona' || SQLERRM; 
        End; 

         nMaxPer := nMaxPer + 1;

        --Secuencia usuario 
        Begin
            select nvl(max(usu_id),0) into nMaxUsu
            from ta_sim_usu_usuario;
             EXCEPTION
                WHEN OTHERS THEN
                Ok := 0;
                oErr := 1;
                oMsg :=  'Error en genera secuencia usuario' || SQLERRM; 
        End; 

         nMaxUsu := nMaxUsu + 1;

      --Validar si el usuario esta en ambas tablas
        BEGIN
          select 'S' into vReg1
            from ta_sim_per_persona
           where per_rut    = iRut
           and per_id in ( select  ta_sim_per_persona_per_id from ta_sim_usu_usuario) ;
        EXCEPTION
          WHEN OTHERS THEN
             vReg1 := 'N';
         END;

        IF vReg1 = 'S' THEN
            Ok := 0;
            oErr := 1;
            oMsg := 'Error al crear usuario. Usuario ya existe. No se ha agregado el registro'; 
        ELSE
             --Usuario esta en la tabla persona y no esta en la tabla usuario  
                BEGIN
                  select 'S',per_id into vReg, nPer_id
                    from ta_sim_per_persona
                   where per_rut    = iRut
                     and per_id not in ( select  ta_sim_per_persona_per_id from ta_sim_usu_usuario) ;
                EXCEPTION
                  WHEN NO_DATA_FOUND THEN
                          vReg := 'N';
                 END;

                IF vReg = 'N' THEN
                      Begin                     
                        Insert into ta_sim_per_persona  (PER_ID,PER_NOMBRE,PER_RUT,PER_EMAIL) values (nMaxPer,iNombre,iRut,iemail); 
                      Exception
                          when others then
                            Ok := 0;
                            oErr := 1;
                            oMsg := 'Error al insertar en ta_sim_per_persona: '||SQLERRM ;
                      End;
     
                      Begin     
                       -- Insert into ta_sim_usu_usuario  (USU_USERNAME,USU_PASSWORD,USU_ID,TA_SIM_PER_PERSONA_PER_ID) values (trim(SUBSTR(iNombre,1,INSTR(iNombre,' ')))||'1',null,nMaxUsu,nMaxPer );
                       Insert into ta_sim_usu_usuario  (USU_USERNAME,USU_PASSWORD,USU_ID,TA_SIM_PER_PERSONA_PER_ID) values (null,null,nMaxUsu,nMaxPer );
                            Ok := 1;
                            oErr := 0;
                            oMsg :=  'Usuario creado: ' ||iNombre ; 
                       Exception
                          when others then
                            Ok := 0;
                            oErr := 1;
                            oMsg := 'Error al insertar en ta_sim_usu_usuario: '||SQLERRM ;
                      End;   
               ELSE

                      Begin
                        Insert into ta_sim_usu_usuario  (USU_USERNAME,USU_PASSWORD,USU_ID,TA_SIM_PER_PERSONA_PER_ID) values (null,null,nMaxUsu, nPer_id );
                       Exception
                          when others then
                            Ok := 0;
                            oErr := 1;
                            oMsg := 'Error al insertar en ta_sim_usu_usuario: '||SQLERRM ;
                      End;   
                        Ok := 1;
                        oErr := 0;
                        oMsg :=  'Usuario creado: ' ||iNombre ; 
                        --  DBMS_OUTPUT.PUT_LINE('voy aqui 2'||oMensaje);
              END IF;
            
        END IF; 
       commit;  
     Exception
                 when others then
                    Ok := 0;
                    oErr := 1;
                    oMsg := 'Error P1_INSERTAR_PER_USU: '||SQLERRM ;
 	END P1_INSERTAR_PER_USU;

--- P2_LISTAR_PER_USU_ROL: procedimiento para listar cantidad de roles asociados a una persona
    PROCEDURE P2_LISTAR_PER_USU_ROL is

     P_CURSOR SYS_REFCURSOR;  
 	 BEGIN
     open P_CURSOR for
     select p.per_nombre Nombre, p.per_rut Rut, COUNT(TA_SIM_ROL_ROL_ROL_ID) Cant_Roles
       from ta_sim_rel_usr_usuario_rol r,
            ta_sim_per_persona p,
            ta_sim_usu_usuario u
        where r.TA_SIM_USU_USUARIO_USU_ID =  u.usu_id
        and p.PER_ID = u.TA_SIM_PER_PERSONA_PER_ID
        GROUP BY p.per_nombre, p.per_rut;

        DBMS_SQL.RETURN_RESULT(P_CURSOR);
   	 END;

--- P3_ELIMINAR_USU_ROL: elimina el rol asociado a un usuario
   PROCEDURE P3_ELIMINAR_USU_ROL (iUsuId in ta_sim_usu_usuario.usu_id%TYPE, Ok out Number,oErr out Number,oMsg out varchar2  ) AS

   vNombre varchar2(100);
   nlog_id number;
   vMsgErr varchar2(500);
   vValidaUser varchar2(1);
  
    BEGIN
    
         Begin
         SELECT p.per_nombre,'S' into vNombre, vValidaUser
           FROM ta_sim_per_persona p,
                ta_sim_usu_usuario u
            where p.PER_ID = u.TA_SIM_PER_PERSONA_PER_ID
            and u.usu_id   = iUsuId;
            Exception
                  when NO_DATA_FOUND then
                        vValidaUser:='N';
                        Ok := 0;
                        oErr := 1;
                        oMsg :=  'Error Usuario no encontrado. ';
          END;    

       --Eliminacion por rol y usuario
      If vValidaUser = 'S' then
           Begin
            delete from ta_sim_rel_usr_usuario_rol r where ta_sim_usu_usuario_usu_id = iUsuId;
             EXCEPTION
                WHEN OTHERS THEN 
                 Ok := 0;
                 oErr := 1;
                 oMsg :=  'Error eliminar (ta_sim_rel_usr_usuario_rol): '||SQLERRM ;
            End; 
            --Eliminacion usuario
            Begin
             delete from ta_sim_usu_usuario u  where usu_id = iUsuId;
             EXCEPTION
                 WHEN OTHERS THEN 
                 Ok := 0;
                 oErr := 1;
                 oMsg :=  'Error eliminar(ta_sim_usu_usuario): '||SQLERRM ;
             End; 
            Ok := 1;
            oErr := 0;
            oMsg := 'Usuario '|| vNombre ||' eliminado existosamente';
           commit;
      End if;      
    Exception
        when others then
        Ok := 0;
        oErr := 1;
        oMsg := 'Error P3_ELIMINAR_USU_ROL: '||SQLERRM ;   
  END P3_ELIMINAR_USU_ROL;

--- P4_INSERTAR_ROL: procedimiento que inserta roles
  PROCEDURE P4_INSERTAR_ROL  (iRol_Nombre in ta_sim_rol_rol.rol_nombre%TYPE , iRol_Ins_Cod in tam_sim_ins_institucion.Ins_Cod%TYPE,irol_flg_ciu in ta_sim_rol_rol. rol_flg_ciu %type,
                                irol_flg_sed in ta_sim_rol_rol. rol_flg_sed %type, irol_flg_fca in ta_sim_rol_rol. rol_flg_fca %type, irol_flg_cdis in ta_sim_rol_rol. rol_flg_cdis%type,
                                irol_flg_car in ta_sim_rol_rol. rol_flg_car%type
                                ,Ok out Number,oErr out Number,oMsg out varchar2 ) AS

     iRol_id ta_sim_rol_rol.rol_id%TYPE; 
     nMaxRol number;
     vReg varchar2(1);
     nlog_id number;
     vMsgErr varchar2(500);
 
    BEGIN
        Begin
         select 'S' into vReg
             from ta_sim_rol_rol
            where rol_nombre  = iRol_Nombre
              and rol_ins_cod = iRol_Ins_Cod;
            Exception
                  when NO_DATA_FOUND then
                       vReg:='N';    
         end;     
       IF vReg = 'S' THEN
           Ok := 0;
           oErr := 1;
           oMsg := 'Error: Rol ya existe. No se ha agregado el registro'; 
        ELSE
               --buscar secuencia rol
                Begin
                select nvl(max(rol_id),0) + 1 into iRol_id
                  from ta_sim_rol_rol;
                 exception
                    when OTHERS then 
                    Ok := 0;
                    oErr := 1;
                    oMsg :=  'Error_1: '||SQLERRM ;
                End; 
               --insertar rol
                Begin
                    select 'S' into vReg
                     from ta_sim_rol_rol
                    where rol_id      = iRol_id
                      and rol_nombre  = iRol_Nombre
                      and rol_ins_cod = iRol_Ins_Cod;
                exception
                          when NO_DATA_FOUND then
                                Begin
                                     Insert into ta_sim_rol_rol (ROL_ID,ROL_NOMBRE,ROL_INS_COD,ROL_FLG_CIU,ROL_FLG_SED,ROL_FLG_FCA,ROL_FLG_CDIS,ROL_FLG_CAR) 
                                          values (iRol_id,iRol_Nombre,iRol_Ins_Cod,irol_flg_ciu,irol_flg_sed,irol_flg_fca,irol_flg_cdis,irol_flg_car);
                                     Ok := 1;
                                     oErr := 0;
                                     oMsg := 'Rol '||iRol_Nombre|| '  creado exitosamente. '; 
                                 exception            
                                       when OTHERS then 
                                       Ok := 0;
                                       oErr := 1;
                                       oMsg :=  'Error en ta_sim_rol_rol '||SQLERRM ; 
                                end;            
                END;
                commit;
        END IF;   
 	END P4_INSERTAR_ROL;

--- P5_ASOCIAR_USU_ROL: procedimiento para relacionar roles a usuarios
 PROCEDURE P5_ASOCIAR_USU_ROL (iUsuId in ta_sim_usu_usuario.usu_id%TYPE, iRolid in ta_sim_rol_rol.rol_id%TYPE ,Ok out Number,oErr out Number,oMsg out varchar2 ) as 
    nMaxRol number;
    nlog_id number;
    vReg varchar2(1);
 
    vrol_nombre varchar2(100);
    vper_nombre varchar2(100);

 BEGIN
       --Buscar nombre del rol
       select rol_nombre into vrol_nombre
       from ta_sim_rol_rol
       where rol_id = iRolid; 

     --Buscar el nombre del usuario
      select p.per_nombre into vper_nombre
        from ta_sim_per_persona p,
             ta_sim_usu_usuario u
        where p.PER_ID = u.TA_SIM_PER_PERSONA_PER_ID
        and u.usu_id = iUsuId;

       BEGIN
            select 'S' into vReg
             from ta_sim_rel_usr_usuario_rol
            where TA_SIM_USU_USUARIO_USU_ID  =iUsuId
              and TA_SIM_ROL_ROL_ROL_ID = iRolid;
        exception
            when OTHERS THEN 
            oMsg:='';
        End; 

        IF vReg = 'S' THEN
            Ok := 0;
            oErr := 1;
            oMsg := 'Error: Rol esta asignado previamente. No se ha podido asignar el Rol'; 
        ELSE
           Begin
              Insert into ta_sim_rel_usr_usuario_rol (USR_FLG_DEFAULT,USR_CIU_COD,USR_SED_COD,USR_CAR_COD,TA_SIM_USU_USUARIO_USU_ID,TA_SIM_ROL_ROL_ROL_ID) values (null,null,null,null,iUsuId,iRolid);
            exception            
               when OTHERS then 
                Ok := 0;
                oErr := 1;
                oMsg :='Error ta_sim_rel_usr_usuario_rol '||SQLERRM ;
           End;
            Ok := 1;
            oErr := 0;
            oMsg := 'El Rol '|| vrol_nombre || ' has sido asignado al usuario ' ||vper_nombre|| ' exitosamente. '; 
            commit;  
         END IF;
   Exception
                 when others then
                     Ok := 0;
                     oErr := 1;
                     oMsg :=  'Error P5_ASOCIAR_USU_ROL: '||SQLERRM ;   
 END P5_ASOCIAR_USU_ROL;

--- P6_LISTAR_ROLES: procedimiento para listar los roles, con las catidades de usuarios,provilegios y cargos
   PROCEDURE P6_LISTAR_ROLES is

     P_CURSOR_ROL SYS_REFCURSOR;  
 	 BEGIN
        open P_CURSOR_ROL for
        select rol_id, rol_nombre,  count(u.ta_sim_usu_usuario_usu_id) cantUsu, count(ta_sim_pri_privilegio_pri_id) cantPri, count(ta_sim_crg_cargo_crg_cod) cantCrg
        from ta_sim_rol_rol r,
             ta_sim_rel_usr_usuario_rol u,
             ta_sim_rel_prr_privilegio_rol p,
             ta_sim_rel_rol_crg d
      where r.rol_id = u.ta_sim_rol_rol_rol_id (+)
       and r.rol_id = p.ta_sim_rol_rol_rol_id (+)
        and r.rol_id = d.ta_sim_rol_rol_rol_id (+)
       GROUP by rol_id, rol_nombre
       order by 1;
    
        DBMS_SQL.RETURN_RESULT(P_CURSOR_ROL);

   	 END P6_LISTAR_ROLES;

--- P7_LISTAR_USU_ROL: procedimiento listar los roles por el Id Usuario
    PROCEDURE P7_LISTAR_USU_ROL(iUsuId in ta_sim_usu_usuario.usu_id%TYPE)  is

     P_CURSOR_USU SYS_REFCURSOR;  
 	 BEGIN
    open P_CURSOR_USU for
      select rr.rol_nombre
        from ta_sim_rel_usr_usuario_rol r,
             ta_sim_per_persona p,
             ta_sim_usu_usuario u,
             ta_sim_rol_rol rr
        where r.ta_sim_usu_usuario_usu_id =  u.usu_id
        and p.per_id = u.ta_sim_per_persona_per_id
        and rr.rol_id = r.ta_sim_rol_rol_rol_id
        and u.usu_id = iUsuId 
        ;
   
       DBMS_SQL.RETURN_RESULT(P_CURSOR_USU);

   	 END P7_LISTAR_USU_ROL;

--- P8_ACTUALIZAR_ROL: procedimiento para actualizar roles
    PROCEDURE P8_ACTUALIZAR_ROL (iRol_Nombre in ta_sim_rol_rol.rol_nombre%TYPE, iRol_Ins_Cod in ta_sim_rol_rol.ROL_INS_COD%TYPE, iRol_id in ta_sim_rol_rol.rol_id%TYPE
                                 ,Ok out Number,oErr out Number,oMsg out varchar2) AS
  
    vReg  varchar2(1);
   
    BEGIN
            select 'S' into vReg
             from ta_sim_rol_rol
            where rol_id = iRol_id;
        IF vReg = 'S' THEN
             Begin
         
              DBMS_OUTPUT.PUT_LINE('VOY 1: ' || iRol_Nombre ||' - ' ||iRol_Ins_Cod );
                 UPDATE ta_sim_rol_rol
                   SET rol_nombre =  iRol_Nombre,
                       rol_ins_cod = iRol_Ins_Cod 
                  WHERE rol_id = iRol_id;
                     Ok := 1;
                     oErr := 0;
                     oMsg := 'Rol Actualizado'; 
                 EXCEPTION
                        WHEN OTHERS THEN 
                        Ok := 0;
                        oErr := 1;
                        oMsg :=  'Error ta_sim_rel_usr_usuario_rol '||SQLERRM ;
                End; 
       END IF;
     commit;
 	END;

--P9_LOGIN_PERSONA_SEL: procedimiento que ingresa el Rut de la persona y devuelve id,nombre,rut,email 
    PROCEDURE P9_LOGIN_PERSONA_SEL (iRut in ta_sim_per_persona.per_rut%TYPE
                                   ,oPer_Id out  ta_sim_per_persona.per_id%TYPE, oRut out ta_sim_per_persona.per_rut%TYPE
                                   ,oNombre out ta_sim_per_persona.per_nombre%TYPE, oEmail out ta_sim_per_persona.per_email%TYPE
                                  ,Ok out Number,oErr out Number,oMsg out varchar2) AS

   vExiste varchar2(1);
    BEGIN
       BEGIN
         SELECT per_id, per_nombre, per_rut, per_email, 'S' INTO oPer_Id, oNombre, oRut, oEmail, vExiste -- Ok, oErr
           FROM ta_sim_per_persona
          WHERE per_rut = iRut 
          ;
        exception
          when NO_DATA_FOUND then
            Ok := 0;
            oErr := 1;
            oMsg :=  'Error: El rut de la Persona no se encuentra registrado.' ;
          when OTHERS THEN 
            Ok := 0;
            oErr := 1;
            oMsg :=  'Error en P9_LOGIN_PERSONA_SEL '||SQLERRM ;
        End; 
        
        if vExiste = 'S' then
        Ok := 1;
        oErr := 0;
        oMsg := 'Rut encontrado';
        end if;
        
        
 	END;

--P10_DOT_PRR_SEL: procedimiento que ingresa el idRol y devuelve los datos de sus privilegios
  PROCEDURE P10_SIM_PRR_SEL  (iRol in ta_sim_rol_rol.rol_id%TYPE)  is

     P_CURSOR SYS_REFCURSOR;  
 	 BEGIN
    open P_CURSOR for
             select  p.pri_id,
                p.pri_id_padre, 
                p.pri_nombre,
                p.pri_desc,
                p.pri_icon,
                p.pri_url_router,
                p.pri_url_path,
                p.pri_method,
                p.pri_flg_menu,
                p.pri_flg_asignable,
                p.pri_orden
            from ta_sim_rol_rol r,
                 ta_sim_rel_prr_privilegio_rol pr,
                 ta_sim_pri_privilegio p
            where r.rol_id = iRol
            and pr.ta_sim_rol_rol_rol_id = r.rol_id
            and pr.ta_sim_pri_privilegio_pri_id = p.pri_id
            ;   
       DBMS_SQL.RETURN_RESULT(P_CURSOR);

   	 END P10_SIM_PRR_SEL;




--P11_LISTAR_USU_ROL_CRG: envia los roles del usuario si tiene puesto asociado
  PROCEDURE P11_LISTAR_USU_ROL_CRG (iUsuId in ta_sim_usu_usuario.usu_id%TYPE)  is

     P_CURSOR_USU SYS_REFCURSOR;  
 	 BEGIN
    open P_CURSOR_USU for
       SELECT DISTINCT rol_id, rol_nombre, rol_ins_cod, rol_flg_ciu, rol_flg_sed, rol_flg_fca, rol_flg_cdis, rol_flg_car
        FROM TA_SIM_CRG_CARGO c, 
             TA_SIM_REL_ROL_CRG g,
             TA_SIM_ROL_ROL r,
             ta_sim_rel_usr_usuario_rol t
        where r.rol_id   =  g.ta_sim_rol_rol_rol_id
        and  c.crg_cod   =  g.ta_sim_crg_cargo_crg_cod
        and t.ta_sim_rol_rol_rol_id = r.rol_id
        and t.ta_sim_usu_usuario_usu_id = iUsuId 
        ;
   
       DBMS_SQL.RETURN_RESULT(P_CURSOR_USU);

   	 END P11_LISTAR_USU_ROL_CRG;

-----
--LISTAR REPORTE
   PROCEDURE LISTAR_CRG_USU_JSON is

     P_CURSOR SYS_REFCURSOR;  
 	 BEGIN
     open P_CURSOR for
     select *
       from TMP_ROL_ROL;

        DBMS_SQL.RETURN_RESULT(P_CURSOR);
   	 END;



---P12_CARGAR_JSON: Se requiere buscar los roles, ingresando un usu_id o mediente un Json que se busca por cargos los roles
PROCEDURE P12_CARGAR_JSON (iUsuId in ta_sim_usu_usuario.usu_id%TYPE, iData in CLOB,Ok out Number,oErr out Number,oMsg out varchar2 ) is
 
    x_ins_cod VARCHAR2(10);
    x_crg_cod VARCHAR2(10);
    x_car_cod VARCHAR2(10);
    x_sed_cod VARCHAR2(10);
    x_IdJson  number(2);
    vExiste   VARCHAR2(1);
    vExiste2  VARCHAR2(1);
    vExiste3   VARCHAR2(1);
    vExiste4   VARCHAR2(1);
    v_rol_id  number;
    aROL_ID	NUMBER;
    aROL_NOMBRE	VARCHAR2(100 BYTE);
    aROL_INS_COD	VARCHAR2(10 BYTE);
    aROL_FLG_CIU	VARCHAR2(1 BYTE);
    aROL_FLG_SED	VARCHAR2(1 BYTE);
    aROL_FLG_FCA	VARCHAR2(1 BYTE);
    aROL_FLG_CDIS	VARCHAR2(1 BYTE);
    aROL_FLG_CAR	VARCHAR2(1 BYTE);
    
    CURSOR C1 IS
      select t.ta_sim_rol_rol_rol_id , r.rol_id, r.rol_nombre, r.rol_ins_cod, r.rol_flg_ciu, r.rol_flg_sed, r.rol_flg_fca, r.rol_flg_cdis, r.rol_flg_car
        from ta_sim_rel_usr_usuario_rol t,
                 TA_SIM_ROL_ROL r,
                 TA_SIM_REL_ROL_CRG g
        where t.ta_sim_usu_usuario_usu_id = iUsuId
          and t.ta_sim_rol_rol_rol_id = r.rol_id
          and r.rol_id   =  g.ta_sim_rol_rol_rol_id(+);

    CURSOR J1 IS
    SELECT TJSON.* 
    FROM TMP_SIM_JSON ,
         JSON_TABLE (json_data, '$[*]'
                       COLUMNS ( ins_cod PATH '$.ins_cod'  
                            , crg_cod PATH '$.crg_cod' 
                            , car_cod PATH '$.car_cod' 
                            , sed_cod PATH '$.sed_cod' 
                            )) AS TJSON
    ;
BEGIN
   --tablas de paso
   DELETE FROM TMP_SIM_JSON;
   DELETE FROM TMP_ROL_ROL;
   
    ---- inserta json en tabla
        begin
          insert into TMP_SIM_JSON (json_id,json_data, json_nom, json_fecreg) 
           values (1,iData,'SIM_INS_CRG_CAR_SED',to_char(sysdate, 'dd-mm-yyyy HH24:MI:SS'));  
          commit;
            Exception
                 when others then
                 Ok := 0;
                 oErr := 1;
                 oMsg := 'Error al insertar TMP_SIM_JSON: '||SQLERRM ;    
         end; 
    --Inserta los roles por el usu_id en tabla de pso
    FOR r1 IN C1 LOOP
       begin
        Insert into TMP_ROL_ROL (ROL_ID,ROL_NOMBRE,ROL_INS_COD,ROL_FLG_CIU,ROL_FLG_SED,ROL_FLG_FCA,ROL_FLG_CDIS,ROL_FLG_CAR) 
              values (r1.ROL_ID,r1.ROL_NOMBRE,r1.ROL_INS_COD,r1.ROL_FLG_CIU,r1.ROL_FLG_SED,r1.ROL_FLG_FCA,r1.ROL_FLG_CDIS,r1.ROL_FLG_CAR);
         commit;   
           Exception
                 when others then
                 Ok := 0;
                 oErr := 1;
                 oMsg := 'Error al insertar TMP_ROL_ROL: '||SQLERRM ;   
         end; 
    end loop;
    
  
  --Buscar los roles por el cargo (json --> campo crg_cod) 
    FOR Reg IN J1 LOOP
      
       x_ins_cod := Reg.ins_cod;
       x_crg_cod := Reg.crg_cod;
       
       Begin
         select ta_sim_rol_rol_rol_id, 'S' into v_rol_id , vExiste
          from TA_SIM_REL_ROL_CRG where ta_sim_crg_cargo_crg_cod = Reg.crg_cod;
          EXCEPTION
               WHEN NO_DATA_FOUND THEN 
             vExiste:='N';
       End;
       
        If  vExiste = 'S' then
       
                begin
                    select 'S' into vExiste3
                    from TMP_ROL_ROL WHERE rol_id = v_rol_id; 
                EXCEPTION
                      WHEN NO_DATA_FOUND THEN 
                        vExiste3:='N';
                end; 
                
                if vExiste3 = 'N' then 
                           begin
                            SELECT ROL_ID,ROL_NOMBRE,ROL_INS_COD,ROL_FLG_CIU,ROL_FLG_SED,ROL_FLG_FCA,ROL_FLG_CDIS,ROL_FLG_CAR, 'S'
                                   into aROL_ID,aROL_NOMBRE,aROL_INS_COD,aROL_FLG_CIU,aROL_FLG_SED,aROL_FLG_FCA,aROL_FLG_CDIS,aROL_FLG_CAR, vExiste4
                                 FROM TA_SIM_ROL_ROL
                                 WHERE rol_id = v_rol_id;
                              EXCEPTION
                                      WHEN NO_DATA_FOUND THEN 
                                        vExiste:='N';
                           end; 
                   
                         if vExiste4 = 'S' then 
                             begin
                              Insert into TMP_ROL_ROL (ROL_ID,ROL_NOMBRE,ROL_INS_COD,ROL_FLG_CIU,ROL_FLG_SED,ROL_FLG_FCA,ROL_FLG_CDIS,ROL_FLG_CAR) 
                              values (aROL_ID,aROL_NOMBRE,aROL_INS_COD,aROL_FLG_CIU,aROL_FLG_SED,aROL_FLG_FCA,aROL_FLG_CDIS,aROL_FLG_CAR);
                              commit;
                                Exception
                                         when others then
                                            Ok := 0;
                                            oErr := 1;
                                            oMsg :=  'Error2 al insertar TMP_ROL_ROL: '||SQLERRM ;    
                                 end; 
                         end if; 
               end if;
         end if;
    END LOOP;    
    --listar la tabla de paso 
       PKG_MANT_ID_NAVEGACION.LISTAR_CRG_USU_JSON;
        Ok := 1;
        oErr := 0;
        oMsg := 'Proceso finalizado con exito';
  Exception
                 when others then
                    Ok := 0;
                    oErr := 1;
                    oMsg := 'Error P12_CARGAR_JSON: '||SQLERRM ; 
END;



END PKG_MANT_ID_NAVEGACION;
/
