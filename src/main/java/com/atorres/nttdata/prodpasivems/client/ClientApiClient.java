package com.atorres.nttdata.prodpasivems.client;

import com.atorres.nttdata.prodpasivems.model.clientms.ClientDto;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Component;
import org.springframework.web.reactive.function.client.WebClient;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

@Component
public class ClientApiClient {
    WebClient client = WebClient.builder()
            .baseUrl("http://localhost:8080/api/client")
            .defaultHeader(HttpHeaders.ACCEPT, MediaType.TEXT_EVENT_STREAM_VALUE)
            .build();

    public Flux<ClientDto> getClients(){
        return client.get()
                .uri("/")
                .retrieve()
                .bodyToFlux(ClientDto.class);
    }

    public Mono<ClientDto> getClientById(String id){
        return client.get()
                .uri("/{id}",id)
                .retrieve()
                .bodyToFlux(ClientDto.class)
                .single();
    }
}
