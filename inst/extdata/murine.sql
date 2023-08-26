drop table if exists mouse_observation;
drop table if exists observation;
drop table if exists mouse;
drop table if exists experimental_group;
drop table if exists experiment;
drop table if exists treatment;

drop trigger if exists update_experiment_last_updated;
drop trigger if exists update_observation_last_updated;


--
-- table definitions
--
create table experiment(
   experiment_id integer primary key,
   name text,  -- defaults to the protocol basename
   purpose text,  -- experimenter's summary of the experiment's intent
   investigator text, -- initials or name of the investigor
   start_date integer,   -- date the experiment was initiated (baseline weights recorded, transplant date)
   comments text,       -- freeform comments parsed from the original protocol
   protocol_file_path text, -- name to the protocol's original file as of August 2021
   type text,
   is_legacy_data boolean,  -- True if data was loaded manually from legacy experiments
   experiment_complete boolean,  -- True if experiment is done; for redcap experiments, this is true when autopsy sheet is finalized
   last_updated datetime default current_timestamp
);

create table experimental_group(
   experimental_group_id integer primary key,
   experiment_id integer,  -- which experiment this is associated with
   group_number integer,  -- the numeric portion of a group (if 2A, group_number is 2)
   legacy_subgroup text,  -- the nonnumeric portion of a group (if 2A, group_cage is A).
   description text, -- group description from protocol
   number integer,  -- number of individuals in group
   organism text,  -- usualy Mus musculus
   organism_line text, -- eg Balb/c
   organism_sex text, -- Female, Male, unknown (for legacy data), NA
   --name text,
   foreign key(experiment_id) references experiment(experiment_id)
      on update cascade
      on delete restrict
);

create table manipulation(
   treatment_id integer primary key,
   experimental_group_id integer,  -- which experimental group this is assocaited with
   name text,  -- drug or manipulation name,
   description text,  -- details about drug, manipulation, etc
   date_administered integer, -- when drug/treatment/manipulation was initiated
   dose integer,  -- how much of <name> was administered
   dose_units text, -- unit for how much of <name> was administered (eg mg/k)
   route text,  -- how <name> was given, eg IV, IP, TV, oral
   foreign key(experimental_group_id) references experimental_group(experimental_group_id)
      on update cascade
      on delete restrict
);

-- see https://jackson.jax.org/rs/444-BUH-304/images/Guide_Mouse_Nomenclature.pdf
create table mouse(
   mouse_id integer primary key,
   experimental_group_id integer, -- which goup the mouse was a part of
   ear_tag text,  -- how the mouse was identitified in the vivarium
   cage text,  -- cage number
   litter text,  -- description of the lineage of the mouse
   date_of_birth integer,
   foreign key(experimental_group_id) references experimental_group(experimental_group_id)
      on update cascade
      on delete restrict
);

create table observation(
   observation_id integer primary key,
   experiment_id integer,  -- which experiment this is associated with
   date integer,
   scores_file_path text,  -- filename of the scores on August 2021
   observer text,          -- who performed the overservation
   is_legacy_data boolean, -- True if data was imported from historical data
   last_updated datetime default current_timestamp,
   foreign key(experiment_id) references experiment(experiment_id)
      on update cascade
      on delete restrict
);

create table mouse_observation(
   mouse_id integer,
   observation_id integer,
   metric_id integer, -- weight, posture score, fur score, activity score, vital status, date_of_death, cause of death,
   value real, -- the numic value of metric;
   notes text,  -- only use this for cause of death for death_date entries
   -- primary key(mouse_id, observation_id, metric_id),
   foreign key(mouse_id) references mouse(mouse_id)
      on update cascade
      on delete restrict,
   foreign key(observation_id) references observation(observation_id)
      on update cascade
      on delete restrict,
   foreign key(metric_id) references metric(metric_id)
      on update cascade
      on delete restrict
);

create table metric(
   metric_id integer primary key,
   name text,
   units text
);

--
-- triggers
--
create trigger update_experiment_last_updated before update on experiment
begin
update experiment
   set last_updated = current_timestamp
   where experiment_id = old.experiment_id;
end;

create trigger update_observation_last_updated before update on observation
begin
update observation
   set last_updated = current_timestamp
   where observation_id = old.observation_id;
end;


insert into metric values(
   1,
   'weight',
   'grams'
);
insert into metric values(
   2,
   'posture',
   'score'
);
insert into metric values(
   3,
   'fur',
   'score'
);
insert into metric values(
   4,
   'activity',
   'score'
);
insert into metric values(
   5,
   'skin',
   'score'
);
insert into metric values(
   6,
   'death_date',
   'julian-encoded date'
);
insert into metric values(
   7,
   'alive',
   '1=alive,0=dead'
);

