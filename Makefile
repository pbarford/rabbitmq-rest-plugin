PACKAGE_NAME=rabbitmq_rest
PACKAGE_VERSION ?=1.0.0
COWLIB_VERSION ?=0.6.1
COWBOY_VERSION ?=0.9.0
RANCH_VERSION ?=0.9.0
NAME ?= rabbitmq_rest
REBAR ?= "./rebar"
CONFIG ?= "priv/app.config"
DIST_DIR ?="./dist"
DEPS_DIR ?="./deps"
RUN := erl -pa ebin -pa deps/*/ebin -smp enable -boot start_sasl -config ${CONFIG} ${ERL_ARGS}

package: all	
	rm -rf $(DIST_DIR)
	mkdir -p $(DIST_DIR)/$(PACKAGE_NAME)-$(PACKAGE_VERSION)/ebin
	mkdir -p $(DIST_DIR)/$(PACKAGE_NAME)-$(PACKAGE_VERSION)/include
	cp ./ebin/* $(DIST_DIR)/$(PACKAGE_NAME)-$(PACKAGE_VERSION)/ebin
	cd $(DIST_DIR) && zip -q -r ./$(PACKAGE_NAME)-$(PACKAGE_VERSION).ez ./$(PACKAGE_NAME)-$(PACKAGE_VERSION)

get_deps:
	${REBAR} get-deps
	rm ./deps/cowboy/src/cowboy_http_websocket.erl

all:    get_deps
	${REBAR} compile

quick:
	${REBAR} skip_deps=true compile

clean:
	${REBAR} clean

quick_clean:
	${REBAR} skip_deps=true clean

run: quick
	if [ -n "${NAME}" ]; then ${RUN} -name ${NAME}@`hostname` -s rabbitmq_rest; \
	else ${RUN} -s rabbitmq_rest; \
	fi

cowboy: all
	mkdir -p $(DIST_DIR)/cowboy-$(COWBOY_VERSION)/ebin
	mkdir -p $(DIST_DIR)/cowboy-$(COWBOY_VERSION)/include
	cp ./$(DEPS_DIR)/cowboy/ebin/* $(DIST_DIR)/cowboy-$(COWBOY_VERSION)/ebin
	cd $(DIST_DIR) && zip -q -r ./cowboy-$(COWBOY_VERSION).ez ./cowboy-$(COWBOY_VERSION)

cowlib: 
	mkdir -p $(DIST_DIR)/cowlib-$(COWLIB_VERSION)/ebin
	mkdir -p $(DIST_DIR)/cowlib-$(COWLIB_VERSION)/include
	cp ./$(DEPS_DIR)/cowlib/ebin/* $(DIST_DIR)/cowlib-$(COWLIB_VERSION)/ebin
	cd $(DIST_DIR) && zip -q -r ./cowlib-$(COWLIB_VERSION).ez ./cowlib-$(COWLIB_VERSION)

ranch: 
	mkdir -p $(DIST_DIR)/ranch-$(RANCH_VERSION)/ebin
	mkdir -p $(DIST_DIR)/ranch-$(RANCH_VERSION)/include
	cp ./$(DEPS_DIR)/ranch/ebin/* $(DIST_DIR)/ranch-$(RANCH_VERSION)/ebin
	cd $(DIST_DIR) && zip -q -r ./ranch-$(RANCH_VERSION).ez ./ranch-$(RANCH_VERSION)

