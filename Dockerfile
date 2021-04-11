# A swipl image with required dependencies installed.
FROM swipl
RUN apt-get update && apt-get install -y graphviz && \
    swipl -g 'pack_install(prolog_graphviz, [interactive(false)])' -t halt
