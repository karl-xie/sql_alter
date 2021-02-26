# sql_alter
a tool to generate some sql statement to upgrade database from old structure to new structure

# example:

oldversion.sql

```
CREATE TABLE foo (
  `id` int default 0 comment 'unique id',
  `name` varchar(20) default '' comment 'name of this row',
  primary key (`id`)
) DEFAULT CHARSET = utf8mb4 COMMENT='table foo';
```

newversion.sql

```
CREATE TABLE foo (
  `id` int default 0 comment 'unique id',
  `number` smallint default 0 comment 'another number',
  `name` varchar(64) default '' comment 'name of this row',
  `content` blob comment 'some binary content',
  `time` int unsigned not null default 0 comment 'add time',
  primary key (`id`),
  key (`time`),
  key (`name`(16))
) DEFAULT CHARSET = utf8 COMMENT='table foo';
```

run the command in shell

```
escript sql_alter -source=file --name="oldversion.sql" -target=file --name="newversion.sql" -out="out.sql"
```

and you will get a file name "out.sql" contains some sql statement

out.sql

```
ALTER TABLE `foo` DEFAULT CHARSET=utf8;
ALTER TABLE `foo` ADD COLUMN `number` smallint(6) DEFAULT '0' COMMENT 'another number' AFTER `id`;
ALTER TABLE `foo` ADD COLUMN `content` blob COMMENT 'some binary content' AFTER `name`;
ALTER TABLE `foo` ADD COLUMN `time` int(10) unsigned NOT NULL DEFAULT '0' COMMENT 'add time' AFTER `content`;
ALTER TABLE `foo` MODIFY COLUMN `name` varchar(64) DEFAULT '' COMMENT 'name of this row' AFTER `number`;
ALTER TABLE `foo` ADD KEY `time` (`time`);
ALTER TABLE `foo` ADD KEY `name` (`name`(16));
```