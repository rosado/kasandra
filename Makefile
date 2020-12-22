pom.xml:
	clj -Spom

kasandra.jar: pom.xml
	clj -M:jar

install: kasandra.jar
	clj -M:install

verify: work-keyspace.cql
	clj -X:dev kasandra.verify/parse-cql :file work-keyspace.cql

clean:
	rm -rf pom.xml kasandra.jar
